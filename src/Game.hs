{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Cell (..),
    Row,
    Grid,
    Game (..),
    Direction (..),
    Hardness,
    mkGame,
    moveCursor,
    flagCell,
    resetGame,
    gameProgress',
    gameSolved',
    clickCell,
  )
where

-- import Data.Function ((&))

import Control.Lens (ix, makeLenses, (%~), (&), (.~), (^.))
import Data.List (nub)
import Data.List.Split (chunksOf)

data Cell
  = Given Int
  | Input Int
  | Note [Int]
  | Empty
  | Hide Int
  | Active Int
  | Flag Int
  | Show_bomb Int
  | Monster Int
  deriving (Eq, Read, Show)

type Row = [Cell]

type Grid = [Row]

data Game = Game
  { _cursor :: (Int, Int),
    _grid :: Grid,
    _leng :: Int,
    _width :: Int,
    _hardness :: Int,
    _mine :: Int,
    _isExplode :: Bool,
    _isOver :: Bool,
    _hp :: Int,
    _ex :: Int,
    _lv :: Int,
    _ne :: Int,
    _msNum :: [Int]
  }
  deriving (Read, Show)

makeLenses ''Game

data Direction
  = North
  | South
  | East
  | West
  deriving (Read, Show)

data Hardness
  = Simple -- 22 lv 1 33| lv2 25|lv3 20 | lv4 13| lv5 6
  | Intermediate --
  | Hard --
  deriving (Read, Show)

mkGame :: Int -> Int -> [Int] -> [Int] -> Game
mkGame d m ms_num xs =
  Game
    { _cursor = (0, 0),
      _grid = chunksOf d $ mkCell <$> xs,
      _leng = d, -- const
      _width = d, -- const
      _hardness = d, -- const
      _mine = m, -- const
      _isExplode = False,
      _isOver = False,
      _ex = 0,
      _hp = 10,
      _lv = 1,
      _ne = 10,
      _msNum = ms_num
    }
  where
    mkCell n = Hide n

moveCursor :: Direction -> Int -> Game -> Game
moveCursor direction distance game =
  (\c -> game {_cursor = c}) $ case direction of
    North -> (x, wrap (y - distance))
    South -> (x, wrap (y + distance))
    East -> (wrap (x + distance), y)
    West -> (wrap (x - distance), y)
  where
    (x, y) = game ^. cursor
    h = game ^. hardness
    wrap n
      | n >= h = n - h -- TODO: n >= game.hardness
      | n < 0 = n + h
      | otherwise = n

transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game = game {_grid = (game ^. grid) & ix x . ix y %~ f}
  where
    (x, y) = game ^. cursor

transformCell' :: (Cell -> Cell) -> Game -> Int -> Int -> Game
transformCell' f game x y = game {_grid = (game ^. grid) & ix x . ix y %~ f}

nextLevelList :: [Int]
nextLevelList = [10, 25, 60, 100, 180, 400]

handleMonster :: Int -> Int -> Game -> Game
handleMonster x y game
  | userLevel >= (- n) = if (experience - n) >= nextLevel then turnMonster (((game & ex .~ (experience - n)) & lv .~ (userLevel + 1)) & ne .~ (nextLevelList !! userLevel)) x y else turnMonster ((game & ex .~ (experience - n)) & (msNum . ix (- n -1)) %~ (\x -> x -1)) x y
  | otherwise = if (experience - n) >= nextLevel then turnMonster (((((game & ex .~ (experience - n)) & lv .~ (userLevel + 1)) & ne .~ (nextLevelList !! userLevel)) & hp .~ (currentHP + n)) & (msNum . ix (- n -1)) %~ (\x -> x -1)) x y else turnMonster (((game & ex .~ (experience - n)) & hp .~ (currentHP + n)) & (msNum . ix (- n -1)) %~ (\x -> x -1)) x y
  where
    Hide n = (game ^. grid) !! x !! y
    turnMonster = transformCell' (\(Hide n) -> Monster n)
    userLevel = game ^. lv
    nextLevel = game ^. ne
    experience = game ^. ex
    currentHP = game ^. hp
    currentMonster = game ^. msNum

handleValidCell :: Int -> Int -> Game -> Game
handleValidCell x y game = case cell of
  Hide n ->
    if (n < 0)
      then handleMonster x y game
      else
        if (n == 0)
          then clickCell (x -1) y (clickCell (x + 1) y (clickCell x (y - 1) (clickCell x (y + 1) (act0 game x y))))
          else transformCell' (\(Hide n) -> Active n) game x y
  c -> transformCell' (const c) game x y
  where
    cell = (game ^. grid) !! x !! y
    act0 = transformCell' (\(Hide n) -> Active n)
    turnMonster = transformCell' (\(Hide n) -> Monster n)
    userLevel = _lv game
    nextLevel = _ne game
    experience = _ex game
    currentHP = _hp game

-- x, y must be the cursor index
clickCell :: Int -> Int -> Game -> Game
clickCell x y game
  | x < 0 || x > (game ^. hardness - 1) || y < 0 || y > (game ^. hardness - 1) = game
  | otherwise = handleValidCell x y game
  where
    cell = (game ^. grid) !! x !! y
    act0 = transformCell' (\(Hide n) -> Active n)
    turnMonster = transformCell' (\(Hide n) -> Monster n)
    userLevel = _lv game
    nextLevel = _ne game
    experience = _ex game
    currentHP = _hp game

-- delete
flagCell :: Game -> Game
flagCell = transformCell $ \case
  Hide n -> Flag n
  Flag n -> Hide n
  c -> c

resetGame :: Game -> Game
resetGame game = game {_grid = fmap (fmap f) (game ^. grid)}
  where
    f = \case
      Given n -> Given n
      _ -> Empty

countRightFlagsRow :: Row -> Int
countRightFlagsRow [] = 0
countRightFlagsRow (x : xs) = case x of
  Flag (-1) -> 1 + countRightFlagsRow xs
  c -> countRightFlagsRow xs

countRightFlagsGrid :: Grid -> Int
countRightFlagsGrid = foldr ((+) . countRightFlagsRow) 0

gameProgress' :: Game -> Int
gameProgress' game = round ((completed / total :: Float) * 100)
  where
    completed = fromIntegral $ countRightFlagsGrid (game ^. grid)
    total = fromIntegral $ (game ^. mine)

gameSolved' :: Game -> Bool
gameSolved' game = countRightFlagsGrid (game ^. grid) == game ^. mine
