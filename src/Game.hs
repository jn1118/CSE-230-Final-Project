{-# LANGUAGE LambdaCase #-}

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
    snapshotGame,
    resetGame,
    gameProgress,
    gameSolved,
    getRegion,
    gameProgress',
    gameSolved',
    clickCell,
  )
where

import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, over, (%~), (.~), (^.))

data Cell
  = Given Int
  | Input Int
  | Note [Int]
  | Empty
  | Hide Int
  | Active Int
  | Flag Int
  | Show_bomb Int
  deriving (Eq, Read, Show)

type Row = [Cell]

type Grid = [Row]

data Game = Game
  { cursor :: (Int, Int),
    grid :: Grid,
    leng :: Int,
    width :: Int,
    hardness :: Int,
    mine :: Int,
    isExplode :: Bool,
    isOver :: Bool,
    previous :: Maybe Game -- TODO: delete
  }
  deriving (Read, Show)

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
  = Simple -- 8
  | Intermediate -- 16
  | Hard -- 20
  deriving (Read, Show)

mkGame :: [Int] -> Game
mkGame xs =
  Game
    { cursor = (0, 2),
      grid = chunksOf 9 $ mkCell <$> xs,
      leng = 9,
      width = 9,
      hardness = 9,
      mine = 10,
      isExplode = False,
      isOver = False,
      previous = Nothing
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
  (\c -> game {cursor = c}) $ case direction of
    North -> (x, wrap (y - distance))
    South -> (x, wrap (y + distance))
    East -> (wrap (x + distance), y)
    West -> (wrap (x - distance), y)
  where
    (x, y) = cursor game
    h = hardness game
    wrap n
      | n >= h = n - h -- TODO: n >= game.hardness
      | n < 0 = n + h
      | otherwise = n

transformCell :: (Cell -> Cell) -> Game -> Game
transformCell f game = game {grid = grid game & ix x . ix y %~ f}
  where
    (x, y) = cursor game

-- TODO: delete this
answerCell :: Int -> Game -> Game
answerCell number = transformCell $ \case
  Given n -> Given n
  _ -> Input number

-- TODO: delete this
toggleNoteCell :: Int -> Game -> Game
toggleNoteCell number = transformCell $ \case
  Given n -> Given n
  Note ns
    | ns == [number] -> Empty
    | number `elem` ns -> Note (filter (/= number) ns)
    | otherwise -> Note (number : ns)
  _ -> Note [number]

transformCell' :: (Cell -> Cell) -> Game -> Int -> Int -> Game
transformCell' f game x y = game {grid = grid game & ix x . ix y %~ f}

-- x, y must be the cursor index
clickCell :: Int -> Int -> Game -> Game
clickCell x y game
  | x < 0 || x > 8 || y < 0 || y > 8 = game
  | otherwise = case cell of
    Hide 0 -> clickCell (x -1) y (clickCell (x + 1) y (clickCell x (y -1) (clickCell x (y + 1) (act0 game x y))))
    Hide (-1) -> game & isOver .~ True
    -- game {isOver = isOver game &  %~ True }
    -- game {isOver = isOver game & _8 .~ True }
    -- transformCell' (\_ -> Show_bomb (-1)) game x y
    Hide n -> transformCell' (\_ -> Active n) game x y
    c -> transformCell' (const c) game x y
  where
    cell = grid game !! x !! y
    act0 = transformCell' (\(Hide n) -> Active n)

flagCell :: Game -> Game
flagCell = transformCell $ \case
  Hide n -> Flag n
  Flag n -> Hide n
  c -> c

-- TODO: delete this
eraseCell :: Game -> Game
eraseCell = transformCell $ \case
  Given n -> Given n
  _ -> Empty

-- TODO: unclear use
snapshotGame :: Game -> Game
snapshotGame game
  | currentGrid /= lastGrid = game {previous = Just game}
  | otherwise = game
  where
    currentGrid = Just $ grid game
    lastGrid = grid <$> previous game

resetGame :: Game -> Game
resetGame game = game {grid = fmap (fmap f) (grid game)}
  where
    f = \case
      Given n -> Given n
      _ -> Empty

resetGame' :: [Int] -> Game
resetGame' = mkGame

gameProgress :: Game -> Int
gameProgress game = round ((completed / total :: Float) * 100)
  where
    cells = concat $ grid game
    completed = fromIntegral $ length $ filter hasValue cells
    total = fromIntegral $ length cells
    hasValue = \case
      Given _ -> True
      Input _ -> True
      Hide _ -> True
      _ -> False

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
    completed = fromIntegral $ countRightFlagsGrid (grid game)
    total = fromIntegral $ mine game

gameSolved :: Game -> Bool
gameSolved game = rowsSolved && columnsSolved && regionsSolved
  where
    rowsSolved = solved $ grid game
    columnsSolved = solved $ getColumns game
    regionsSolved = solved $ getRegionsFlat game
    solved = all (\ns -> nub ns == ns) . fmap (fmap getNum)
    getNum = \case
      Given n -> Just n
      Input n -> Just n
      _ -> Nothing

gameSolved' :: Game -> Bool
gameSolved' game = countRightFlagsGrid (grid game) == mine game

getColumns :: Game -> [[Cell]]
getColumns game =
  [[grid game !! row !! column | row <- [0 .. 8]] | column <- [0 .. 8]]

getRegion :: Int -> Game -> [[Cell]]
getRegion number game =
  [[grid game !! row !! col | col <- [x .. x + 2]] | row <- [y .. y + 2]]
  where
    x = (number `mod` 3) * 3
    y = number - (number `mod` 3)

getRegionsFlat :: Game -> [[Cell]]
getRegionsFlat game = [concat $ getRegion x game | x <- [0 .. 8]]

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