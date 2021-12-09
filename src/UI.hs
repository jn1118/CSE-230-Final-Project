{-# LANGUAGE TemplateHaskell #-}
module UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (VtyEvent),
    EventM,
    Next,
    Padding (Max, Pad),
    Widget,
    attrMap,
    attrName,
    bg,
    continue,
    defaultMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    padLeftRight,
    padRight,
    setAvailableSize,
    str,
    vBox,
    withAttr,
    withBorderStyle,
    withDefAttr,
    (<+>),
    (<=>),
  )
import Brick.Widgets.Border (border, borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold, unicodeRounded)
import Brick.Widgets.Center (center)
import Data.Char (digitToInt)
import Data.List (intersperse, nub)
import System.Random (newStdGen, Random (randomRs))
import System.Random.Shuffle (shuffle', shuffle)

import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import FileIO
import Game
import qualified Graphics.Vty as V
import Control.Lens (makeLenses,ix, (%~), (.~) , (&), (^.))
import System.Directory (doesFileExist)

makeLenses ''Game

styleCursor, styleCellGiven, styleCellInput, styleCellNote :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"
styleCellInput = attrName "styleCellInput"
styleCellNote = attrName "styleCellNote"
styleSolved = attrName "styleSolved"
styleUnsolved = attrName "styleUnsolved"
styleHiddenBg :: AttrName
styleHiddenBg = attrName "styleHiddenBg"
styleCursorFc :: AttrName
styleCursorFc = attrName "styleCursorFc"
styleMonsterLv1, styleMonsterLv2, styleMonsterLv3, styleMonsterLv4, styleMonsterLv5 :: AttrName
styleMonsterLv1 = attrName "styleMonsterLv1"
styleMonsterLv2 = attrName "styleMonsterLv2"
styleMonsterLv3 = attrName "styleMonsterLv3"
styleMonsterLv4 = attrName "styleMonsterLv4"
styleMonsterLv5 = attrName "styleMonsterLv5"



attributes :: AttrMap
attributes =
  attrMap
    V.defAttr
    [ (styleCursor, bg V.brightWhite),
      (styleCellGiven, V.defAttr),
      (styleCellInput, fg V.blue),
      (styleCellNote, fg V.yellow),
      (styleSolved, fg V.green),
      (styleUnsolved, fg V.red),
      (styleHiddenBg, fg V.black),
      (styleMonsterLv1, fg V.cyan),
      (styleMonsterLv2, fg V.red),
      (styleMonsterLv3, fg V.magenta),
      (styleMonsterLv4, fg V.yellow),
      (styleMonsterLv5, fg V.green)
    ]

handleEvent :: Game -> BrickEvent () e -> EventM () (Next Game)
handleEvent game (VtyEvent (V.EvKey key [V.MCtrl])) =
  case key of
    -- Quit
    V.KChar 'c' -> halt game
    -- Reset
    V.KChar 'r' -> continue . resetGame $ game
    -- Other
    _ -> continue game
handleEvent game (VtyEvent (V.EvKey key [V.MShift])) =
  continue $ case key of
    V.KUp -> moveCursor North 3 game
    V.KDown -> moveCursor South 3 game
    V.KLeft -> moveCursor West 3 game
    V.KRight -> moveCursor East 3 game
    _ -> game
handleEvent game (VtyEvent (V.EvKey key [])) =
  continue $ case key of
    -- Move by cell
    V.KUp -> moveCursor North 1 game
    V.KDown -> moveCursor South 1 game
    V.KLeft -> moveCursor West 1 game
    V.KRight -> moveCursor East 1 game
    -- click
    V.KChar 'd' -> clickCell x y game
    --flag
    V.KChar 'f' -> flagCell game
    -- Other
    _ -> game
  where
    (x, y) = _cursor game
handleEvent game _ = continue game

-- highlight the chosen cell
highlightCursor :: Game -> [[Widget ()]] -> [[Widget ()]]
highlightCursor game widgets =
  widgets
    & ix x
      . ix y
    %~ withDefAttr styleCursor
  where
    (x, y) = _cursor game

drawCell :: Game -> Cell -> Widget ()
drawCell game cell =
  center $
    if _isOver game
      then case cell of
        Hide (-1) -> withAttr styleCellGiven . str $ "Bomb!!"
        _ -> str " "
      else case cell of
        Hide _ -> withAttr styleHiddenBg . str $ "." --withAttr styleCellGiven . str $ show x 保存好自己的x但是里面具体是啥不用显示，可以表示为一个灰色方块
        Active x -> withAttr styleCellGiven . str $ show x
        Show_bomb _ -> withAttr styleCellGiven . str $ "Bomb!!"
        Flag _ -> withAttr styleCellGiven . str $ "⚐"
        Given x -> withAttr styleCellGiven . str $ show x
        Input x -> withAttr styleCellInput . str $ show x
        Note xs ->
          fmap str xs'
            & chunksOf 3
            & fmap hBox
            & vBox
            & withAttr styleCellNote
          where
            xs' = fmap f [1 .. (game ^. hardness)]
            f x = if x `elem` xs then show x else " "
        Monster x ->  
          case x of
           (-1) -> withAttr styleMonsterLv1 . str $ "☥"
           (-2) -> withAttr styleMonsterLv2 . str $ "♙"
           (-3) -> withAttr styleMonsterLv3 . str $ "♜"
           (-4) -> withAttr styleMonsterLv4 . str $ "♘"
           (-5) -> withAttr styleMonsterLv5 . str $ "♕"
           _    -> str " "
        Empty -> str " "

drawGrid :: Game -> Widget ()
drawGrid game =
  _grid game
    & fmap (fmap (drawCell game)) -- render Cell
    & fmap (fmap $ hLimit 37)
    & highlightCursor game -- 显示被选择的位置
    & fmap (intersperse (withBorderStyle unicode hBorder))
    & fmap vBox -- [Widget]
    & intersperse (withBorderStyle unicode vBorder)
    & hBox
    & border
    & withBorderStyle unicodeBold
    & setAvailableSize (73, 37)
    & padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  [ "move:    ←↓↑→",
    "open:    d",
    "flog:    f",
    "undo:    ctrl + z / u",
    "reset:   ctrl + r",
    "quit:    ctrl + c"
  ]
    & unlines
    & str
    & padLeftRight 1
    & borderWithLabel (str " Instruction ")
    & withBorderStyle unicodeRounded
    & setAvailableSize (31, 12)

drawDebug :: Game -> Widget ()
drawDebug game =
  [ "cursor:    (" <> show x <> ", " <> show y <> ")",
    "HP:        " <> show (game ^. hp),
    "LV:        " <> show (game ^. lv),
    "EX:        " <> show (game ^. ex),
    "NE:        " <> show (game ^. ne)
  ]
    & unlines
    & str
    & padRight Max
    & padLeftRight 1
    & setAvailableSize (31, 6)
    & borderWithLabel (str " Debug ")
    & withBorderStyle unicodeRounded
  where
    -- & hLimit 31
    (x, y) = _cursor game

drawSolved :: Game -> Widget ()
drawSolved game
  | completed && solved =
    str "SOLVED" & withAttr styleSolved & commonModifier
  | not solved =
    str "INCORRECT" & withAttr styleUnsolved & commonModifier
  | otherwise = emptyWidget
  where
    completed = gameProgress' game == 100
    solved = gameSolved' game
    commonModifier =
      setAvailableSize (31, 3)
        . withBorderStyle unicodeRounded
        . border
        . center

drawUI :: Game -> Widget ()
drawUI game =
  -- <+> Horizontal box layout
  -- <=> vertical box layout
  drawGrid game
    <+> ( drawHelp
            <=> drawDebug game
            <=> drawSolved game
        )

app :: App Game e ()
app =
  App
    { appDraw = \x -> [drawUI x],
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap = const attributes
    }

containBomb :: (Eq a, Num p, Num a) => [[a]] -> Int -> Int -> Int -> p
containBomb nums x y n
 | x <0 || y < 0 || x >= n || y >=n = 0
 | nums!!x!!y == -1 = 1
 | otherwise = 0

fillNum :: (Eq a, Num a) => Int -> (Int, Int) -> [[a]] -> [[a]]
fillNum n pos nums
  | nums !!x!!y == -1 = nums
  | otherwise = nums & ix x . ix y .~ (containBomb nums (x-1) (y-1) n) +
                                            (containBomb nums (x-1) (y) n) +
                                            (containBomb nums (x-1) (y+1) n)+
                                            (containBomb nums (x) (y+1) n)+
                                            (containBomb nums (x) (y-1) n)+
                                            (containBomb nums (x+1) (y-1) n)+
                                            (containBomb nums (x+1) (y+1) n)+
                                            (containBomb nums (x+1) (y) n)
  where x = fst pos
        y = snd pos

fillMine :: (Eq a, Num a) => (Int, Int) -> [[a]] -> [[a]]
fillMine pos nums = nums & ix x . ix y .~ (-1)
 where
   x = fst pos
   y = snd pos




geneInit n mines= do
      g <- newStdGen
      let initList = [1 | _ <- [1..33]] ++ [2 | _ <- [1..27]] ++ [3 | _<- [1.. 20]] ++ [4 | _<- [1..13]] ++ [5 | _<- [1..6]] ++ [0 | _<-[1..(n*n-33-27-20-13-6)]]
      let shuffledList = shuffle' initList (length initList) g
      let nums = chunksOf n shuffledList
      -- let a  = take mines . nub $ (randomRs (0,n*n-1) g :: [Int])
      -- let b = [(num `div` n, num `mod` n) | num <- a]
      -- let nums = [[0 | _ <- [1..n]] | _<- [1..n]]
      -- let nums' =  foldr fillMine nums b
      let poss = [(p, q) | p <- [0..(n-1)], q <- [0..(n-1)]]
      let nums'' = foldr (fillNum n) nums poss
      return nums''

-- >>> geneInit 22 3
-- [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]


main :: IO ()
main = do
  putStr $
    unlines
      [ "Mine Sweeper",
        "  1) Load demo game",
        "  2) Load file",
        "  3) Load autosave",
        "  4) Load game string",
        "  *) Quit"
      ]
  response <- prompt "> "
  case head' response of
    '1' -> do
      initState <- geneInit 16 10
      let state = concat initState
      endGame <- defaultMain app (mkGame 16 10 state)
      promptSave endGame
      saveGame "autosave.sudoku" endGame
    '2' -> do
      filename <- prompt "Filename: "
      exists <- doesFileExist filename
      if exists
        then do
          game <- loadGame filename
          endGame <- defaultMain app game
          promptSave endGame
          saveGame "autosave.sudoku" endGame
        else putStrLn $ "File '" <> filename <> "' does not exist"
    '3' -> do
      exists <- doesFileExist "autosave.sudoku"
      if exists
        then do
          game <- loadGame "autosave.sudoku"
          endGame <- defaultMain app game
          promptSave endGame
          saveGame "autosave.sudoku" endGame
        else putStrLn "File 'autosave.sudoku' does not exist"
    '4' -> do
      gameString <- prompt "Game string: "
      let game = (mkGame 9 10 . fmap digitToInt) gameString
      endGame <- defaultMain app game
      promptSave endGame
      saveGame "autosave.sudoku" endGame
    _ -> putStrLn "Quitting..."
  where
    head' [] = ' '
    head' x = head x

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
