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
    snapshotGame,
    resetGame,
    gameProgress,
    gameSolved,
    getRegion,
    clickCell,
  )
where

import Data.Function ((&))
import Data.List (nub)
import Data.List.Split (chunksOf)
import Lens.Micro (ix, (%~))

data Cell
  = Given Int
  | Input Int
  | Note [Int]
  | Empty
  | Hide Int
  | Active Int
  | Flag Int
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
mkGame xs = Game
  { cursor = (0, 2)
  , grid = chunksOf 9 $ mkCell <$> xs
  , leng = 9
  , width = 9
  , hardness = 9
  , mine = 10
  , previous = Nothing
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
    h      = hardness game
    wrap n
      | n >= h    = n - h -- TODO: n >= game.hardness
      | n < 0     = n + h
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

-- clickCell :: Game -> Game
-- clickCell  = transformCell $ \case
--   Hide (-1) -> Empty
--   Hide n -> Active n
--   c      -> c
  -- TODO click mine -> end game
  -- TODO click zero -> BFS

transformCell' :: (Cell -> Cell) -> Game -> Int -> Int -> Game
transformCell' f game x y = game { grid = grid game & ix x . ix y %~ f }

-- x, y must be the cursor index
clickCell ::Int -> Int -> Game -> Game
clickCell (-1) _ game = game
clickCell 9 _ game = game
clickCell _ (-1) game = game
clickCell _ 9 game = game
clickCell x y game = case cell of
  -- make (x, y) Active 0, clickCell (x+1, y),(x-1, y), (x, y+1), (x, y-1)
  -- Hide 0    -> clickCell x (max (y-1) 0) (clickCell x (min (y+1) 8) (clickCell (max (x-1) 0) y (clickCell (min (x+1) 8) y (act0 game x y))))
  Hide 0    -> clickCell (x-1) y (clickCell (x+1) y (clickCell x (y-1) (clickCell x (y+1) (act0 game x y))))
  Hide (-1) -> transformCell (\_ -> Empty) game
  Hide n    -> transformCell (\_ -> Active n) game
  c         -> transformCell (\_ -> c) game
  where
    -- (x, y) = cursor game
    cell   = (grid game)!!x!!y
    act0 = transformCell' (\_ -> Active 0)

flagCell :: Game -> Game
flagCell = transformCell $ \case
  Hide n -> Flag n
  Flag n -> Hide n
  c      -> c

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
simple = [-1,1,0,0,0,0,1,-1,1
          ,1,1,0,0,1,2,3,2,1
          ,1,1,1,0,1,-1,-1,2,2
          ,1,-1,2,1,2,2,3,-1,1
          ,1,1,2,-1,1,0,1,1,1
          ,0,0,1,1,1,0,0,0,0
          ,0,1,2,2,2,1,1,0,0
          ,0,1,-1,-1,2,-1,1,0,0
          ,0,1,2,2,2,1,1,0,0]