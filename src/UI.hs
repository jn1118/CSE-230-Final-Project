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
import Control.Lens (ix, makeLenses, (%~), (&), (.~), (^.))
import Data.Char (digitToInt)
import Data.List (intersperse, nub)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import FileIO
import Game
import qualified Graphics.Vty as V
import System.Directory (doesFileExist)
import System.Random (Random (randomRs), newStdGen)
import System.Random.Shuffle (shuffle, shuffle')

makeLenses ''Game

styleCursor, styleCellGiven :: AttrName
styleSolved, styleUnsolved :: AttrName
styleCursor = attrName "styleCursor"
styleCellGiven = attrName "styleCellGiven"

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
    -- Other
    _ -> continue game
handleEvent game (VtyEvent (V.EvKey key [])) =
  continue $ case key of
    -- Move by cell
    V.KUp -> if game ^. hp > 0 then moveCursor North 1 game else moveCursor North 0 game
    V.KDown -> if game ^. hp > 0 then moveCursor South 1 game else moveCursor North 0 game
    V.KLeft -> if game ^. hp > 0 then moveCursor West 1 game else moveCursor North 0 game
    V.KRight -> if game ^. hp > 0 then moveCursor East 1 game else moveCursor North 0 game
    -- click
    V.KChar 'd' -> clickCell x y game
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
    if (game ^. hp <= 0)
      then case cell of
        Hide x ->
          case x of
            (-1) -> withAttr styleMonsterLv1 . str $ "☥"
            (-2) -> withAttr styleMonsterLv2 . str $ "♙"
            (-3) -> withAttr styleMonsterLv3 . str $ "♜"
            (-4) -> withAttr styleMonsterLv4 . str $ "♘"
            (-5) -> withAttr styleMonsterLv5 . str $ "♕"
            _ -> str " "
        Monster y ->
          case y of
            (-1) -> withAttr styleMonsterLv1 . str $ "☥"
            (-2) -> withAttr styleMonsterLv2 . str $ "♙"
            (-3) -> withAttr styleMonsterLv3 . str $ "♜"
            (-4) -> withAttr styleMonsterLv4 . str $ "♘"
            (-5) -> withAttr styleMonsterLv5 . str $ "♕"
            _ -> str " "
        _ -> str " "
      else case cell of
        Hide _ -> withAttr styleHiddenBg . str $ "." --withAttr styleCellGiven . str $ show x
        Active x -> withAttr styleCellGiven . str $ show x
        Monster x ->
          case x of
            (-1) -> withAttr styleMonsterLv1 . str $ "☥"
            (-2) -> withAttr styleMonsterLv2 . str $ "♙"
            (-3) -> withAttr styleMonsterLv3 . str $ "♜"
            (-4) -> withAttr styleMonsterLv4 . str $ "♘"
            (-5) -> withAttr styleMonsterLv5 . str $ "♕"
            _ -> str " "
        Empty -> str " "

drawGrid :: Game -> Widget ()
drawGrid game =
  _grid game
    & fmap (fmap (drawCell game)) -- render Cell
    & fmap (fmap $ hLimit 37)
    & highlightCursor game
    & fmap (intersperse (withBorderStyle unicode hBorder))
    & fmap vBox -- [Widget]
    & intersperse (withBorderStyle unicode vBorder)
    & hBox
    & border
    & withBorderStyle unicodeBold
    & setAvailableSize (83, 47)
    & padRight (Pad 1)

drawHelp :: Widget ()
drawHelp =
  [ "move:    ←↓↑→",
    "enter:    d",
    "quit:    ctrl + c",
    " ",
    "Monster",
    "LV1: Blue ☥",
    "LV2: red ♙",
    "LV3: magenta ♜",
    "LV4: yellow ♘",
    "LV5: Green ♕"
  ]
    & unlines
    & str
    & padLeftRight 1
    & borderWithLabel (str " Instruction ")
    & withBorderStyle unicodeRounded
    & setAvailableSize (45, 45)

drawDebug :: Game -> Widget ()
drawDebug game =
  [ "HP:        " <> show (game ^. hp),
    "LV:        " <> show (game ^. lv),
    "EX:        " <> show (game ^. ex),
    "NE:        " <> show (game ^. ne)
  ]
    & unlines
    & str
    & padRight Max
    & padLeftRight 1
    & setAvailableSize (45, 6)
    & borderWithLabel (str " Profile ")
    & withBorderStyle unicodeRounded

drawLeftMonster :: Game -> Widget ()
drawLeftMonster game =
  [ "L1 Monster Left:        " <> show ((game ^. msNum) !! 0),
    "L2 Monster Left:        " <> show ((game ^. msNum) !! 1),
    "L3 Monster Left:        " <> show ((game ^. msNum) !! 2),
    "L4 Monster Left:        " <> show ((game ^. msNum) !! 3),
    "L5 Monster Left:        " <> show ((game ^. msNum) !! 4)
  ]
    & unlines
    & str
    & padRight Max
    & padLeftRight 1
    & setAvailableSize (45, 6)
    & borderWithLabel (str " Monsters left: ")
    & withBorderStyle unicodeRounded
  where
    -- & hLimit 31
    (x, y) = _cursor game

drawLogo :: Widget ()
drawLogo =
  [ "   *             *       )     )    )   ",
    " (  `    (     (  `   ( /(  ( /( ( /(   ",
    " )\\))(   )\\    )\\))(  )\\()) )\\()))\\())  ",
    "((_)()((((_)( ((_)()\\((_)\\ ((_)\\((_)\\   ",
    "(_()((_)\\ _ )\\(_()((_) ((_) _((_) ((_)  ",
    "|  \\/  (_)_\\(_)  \\/  |/ _ \\| \\| |/ _ \\  ",
    "| |\\/| |/ _ \\ | |\\/| | (_) | .` | (_) | ",
    "|_|  |_/_/ \\_\\|_|  |_|\\___/|_|\\_|\\___/  "
  ]
    & unlines
    & str
    & padLeftRight 1
    & borderWithLabel (str " MAMONO ")
    & withBorderStyle unicodeRounded
    & setAvailableSize (45, 12)

drawSolved :: Game -> Widget ()
drawSolved game
  | solved =
    str "YOU DID IT!!" & withAttr styleSolved & commonModifier
  | game ^. hp <= 0 =
    str "FAILED!!" & withAttr styleUnsolved & commonModifier
  | otherwise = emptyWidget
  where
    solved = gameSolved' game
    commonModifier =
      setAvailableSize (45, 3)
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
            <=> drawLeftMonster game
            <=> drawLogo
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

containBomb :: (Num p, Ord p) => [[p]] -> Int -> Int -> Int -> p
containBomb nums x y n
  | x < 0 || y < 0 || x >= n || y >= n = 0
  | nums !! x !! y < 0 = - (nums !! x !! y)
  | otherwise = 0

fillNum :: (Eq a, Num a, Ord a) => Int -> (Int, Int) -> [[a]] -> [[a]]
fillNum n pos nums
  | nums !! x !! y < 0 = nums
  | otherwise =
    nums & ix x . ix y .~ (containBomb nums (x -1) (y -1) n)
      + (containBomb nums (x -1) (y) n)
      + (containBomb nums (x -1) (y + 1) n)
      + (containBomb nums (x) (y + 1) n)
      + (containBomb nums (x) (y -1) n)
      + (containBomb nums (x + 1) (y -1) n)
      + (containBomb nums (x + 1) (y + 1) n)
      + (containBomb nums (x + 1) (y) n)
  where
    x = fst pos
    y = snd pos

geneInit :: (Num a, Ord a) => Int -> IO [[a]]
geneInit n = do
  g <- newStdGen
  let initList = [-1 | _ <- [1 .. 24]] ++ [-2 | _ <- [1 .. 14]] ++ [-3 | _ <- [1 .. 10]] ++ [-4 | _ <- [1 .. 6]] ++ [-5 | _ <- [1 .. 3]] ++ [0 | _ <- [1 .. (n * n -24 -14 -10 -6 -3)]]
  let shuffledList = shuffle' initList (length initList) g
  let nums = chunksOf n shuffledList
  let poss = [(p, q) | p <- [0 .. (n -1)], q <- [0 .. (n -1)]]
  let nums'' = foldr (fillNum n) nums poss
  return nums''

-- >>> geneInit 22 3
-- [[-1,1,3,3,9,-3,-4,8,3,-3,3,0,0,3,-3,-2,5,-2,3,1,1,0],[5,5,7,-3,9,-3,-4,11,6,6,3,0,0,8,10,11,-1,3,3,-1,1,0],[4,-4,7,3,6,9,9,11,-3,9,4,6,4,9,-5,6,1,1,1,2,2,1],[8,8,4,0,4,6,-2,8,-2,9,-4,9,-2,-2,8,6,1,0,0,3,-1,3],[-4,4,0,0,4,-4,7,-1,5,8,7,-3,7,4,3,-1,1,0,1,8,-2,8],[6,6,1,1,5,4,5,3,-2,2,4,4,4,1,2,2,1,0,1,-1,-4,-1],[-2,2,1,-1,3,2,4,6,6,2,1,-1,6,6,-1,3,2,1,2,13,-3,12],[2,4,3,7,-2,4,4,-4,4,0,3,8,-5,7,6,-2,7,5,-1,9,-4,7],[2,4,-2,6,-2,5,5,5,4,0,2,-2,7,6,-1,-2,8,-3,-1,7,5,4],[2,-2,4,4,2,5,-1,5,2,0,2,2,2,1,4,-1,7,5,6,-1,1,0],[7,7,7,0,1,4,-2,-2,2,0,0,0,0,0,1,1,3,-1,3,1,1,0],[6,-5,8,2,6,-1,14,8,6,0,0,0,0,0,0,0,5,-1,5,0,3,3],[9,-1,-2,7,8,-5,15,-4,9,2,0,0,3,3,4,1,5,-3,4,0,4,-3],[5,-3,-5,10,7,-2,20,-3,-2,5,3,3,3,-3,4,-1,4,3,3,0,4,-1],[4,-1,-3,11,2,4,-2,-4,9,5,-3,3,3,3,4,1,1,0,0,0,1,1],[1,7,-3,6,0,2,6,6,4,4,4,4,0,2,2,2,0,0,0,0,0,0],[1,9,9,8,0,0,0,2,2,3,-1,1,0,2,-2,2,3,3,6,3,3,0],[2,-1,-5,7,0,0,0,2,-2,4,2,4,3,5,2,2,3,-3,6,-3,3,0],[2,-1,-2,7,0,0,0,2,3,-1,1,3,-3,3,0,0,3,3,6,3,4,1],[2,4,8,6,4,0,0,0,1,2,2,5,4,4,0,0,0,0,0,0,1,-1],[1,-1,6,-4,6,2,2,2,2,7,-1,6,-1,1,0,0,0,0,0,0,1,1],[1,2,-1,5,6,-2,2,2,-2,7,-4,6,1,1,0,0,0,0,0,0,0,0]]

main :: IO Game
main = do
  putStr $
    unlines
      [ "'##::::'##::::'###::::'##::::'##::'#######::'##::: ##::'#######::",
        " ###::'###:::'## ##::: ###::'###:'##.... ##: ###:: ##:'##.... ##:",
        " ####'####::'##:. ##:: ####'####: ##:::: ##: ####: ##: ##:::: ##:",
        " ## ### ##:'##:::. ##: ## ### ##: ##:::: ##: ## ## ##: ##:::: ##:",
        " ##. #: ##: #########: ##. #: ##: ##:::: ##: ##. ####: ##:::: ##:",
        " ##:.:: ##: ##.... ##: ##:.:: ##: ##:::: ##: ##:. ###: ##:::: ##:",
        " ##:::: ##: ##:::: ##: ##:::: ##:. #######:: ##::. ##:. #######::",
        "..:::::..::..:::::..::..:::::..:::.......:::..::::..:::.......:::",
        "MAMONO (monster) Sweeper -- choose difficulty",
        "  1) EASY",
        "  2) MEDIUM",
        "  3) HARD",
        "  other) MEDIUM"
      ]
  response <- prompt "> "
  case head' response of
    '1' -> do
      initState <- geneInit 16
      let state = concat initState
      defaultMain app (mkGame 20 16 10 [24, 14, 10, 6, 3] state)
    '2' -> do
      initState <- geneInit 16
      let state = concat initState
      defaultMain app (mkGame 10 16 10 [24, 14, 10, 6, 3] state)
    '3' -> do
      initState <- geneInit 16
      let state = concat initState
      defaultMain app (mkGame 5 16 10 [24, 14, 10, 6, 3] state)
    _ -> do
      initState <- geneInit 16
      let state = concat initState
      defaultMain app (mkGame 10 16 10 [24, 14, 10, 6, 3] state)
  where
    head' [] = ' '
    head' x = head x

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
