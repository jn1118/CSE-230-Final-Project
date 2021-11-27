module Main where

import Brick ( str, Widget, simpleMain )

ui :: Widget()
ui = str "Hello, World!"



main :: IO ()
main = simpleMain ui


