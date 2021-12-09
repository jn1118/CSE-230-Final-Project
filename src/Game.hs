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
import Data.List (nub)
import Data.List.Split (chunksOf)
import Control.Lens (makeLenses, ix, (%~), (.~) , (&), (^.))


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
    _ne :: Int
  }
  deriving (Read, Show)

makeLenses ''Game

data Direction
  = North
  | South
  | East
  | West
  deriving (Read, Show)

-- mkGame :: [Int] -> Game
-- mkGame xs = Game
--   { cursor = (4, 4)
--   , grid = chunksOf 9 $ mkCell <$> xs
--   , previous = Nothing
--   }
--   where mkCell 0 = Empty
--         mkCell n = Given n

data Hardness
  = Simple -- 22 lv 1 33| lv2 25|lv3 20 | lv4 13| lv5 6
  | Intermediate -- 
  | Hard -- 
  deriving (Read, Show)

mkGame :: Int -> Int -> [Int] -> Game
mkGame d m xs =
  Game
    { _cursor = (0, 2),
      _grid = chunksOf d $ mkCell <$> xs,
      _leng = d,
      _width = d,
      _hardness = d,
      _mine = m,
      _isExplode = False,
      _isOver = False,
      _ex = 0,
      _hp = 10,
      _lv = 1,
      _ne = 10
    }
  where
    mkCell n = Hide n

-- moveCursor :: Direction -> Int -> Game -> Game
-- moveCursor direction distance game =
--   (\c -> game { cursor = c }) $ case direction of
--     North -> (x, wrap (y - distance))
--     South -> (x, wrap (y + distance))
--     East  -> (wrap (x + distance), y)
--     West  -> (wrap (x - distance), y)
--   where
--     (x, y) = cursor game
--     wrap n
--       | n >= 9    = n - 9 -- TODO: n >= game.hardness
--       | n < 0     = n + 9
--       | otherwise = n

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
      | n >= h = n -  h-- TODO: n >= game.hardness
      | n < 0 = n + h
      | otherwise = n

transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game = game {_grid = (game ^. grid) & ix x . ix y %~ f}
  where
    (x, y) = game ^. cursor

transformCell' :: (Cell -> Cell) -> Game -> Int -> Int -> Game
transformCell' f game x y = game {_grid = (game ^. grid) & ix x . ix y %~ f}

-- x, y must be the cursor index
clickCell :: Int -> Int -> Game -> Game
clickCell x y game
  | x < 0 || x > (game ^. hardness - 1) || y < 0 || y > (game ^. hardness - 1) = game
  | otherwise = case cell of
    Hide 0 -> clickCell (x -1) y (clickCell (x + 1) y (clickCell x (y -1) (clickCell x (y + 1) (act0 game x y))))
    Hide (-1) -> game & isOver .~ True
    -- game {isOver = isOver game &  %~ True }
    -- game {isOver = isOver game & _8 .~ True }
    -- transformCell' (\_ -> Show_bomb (-1)) game x y
    Hide n -> transformCell' (\_ -> Active n) game x y
    c -> transformCell' (const c) game x y
  where
    cell = (game ^. grid) !! x !! y
    act0 = transformCell' (\(Hide n) -> Active n)

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

simple :: [Int]
simple =
  [ -1,
    1,
    0,
    0,
    0,
    0,
    1,
    -1,
    1,
    1,
    1,
    0,
    0,
    1,
    2,
    3,
    2,
    1,
    1,
    1,
    1,
    0,
    1,
    -1,
    -1,
    2,
    2,
    1,
    -1,
    2,
    1,
    2,
    2,
    3,
    -1,
    1,
    1,
    1,
    2,
    -1,
    1,
    0,
    1,
    1,
    1,
    0,
    0,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    1,
    2,
    2,
    2,
    1,
    1,
    0,
    0,
    0,
    1,
    -1,
    -1,
    2,
    -1,
    1,
    0,
    0,
    0,
    1,
    2,
    2,
    2,
    1,
    1,
    0,
    0
  ]
