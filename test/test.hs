module Main where

import Control.Lens
import Game
import System.Exit
import Test.HUnit

main :: IO ()
main =
  do
    putStrLn "MAMONO TESTING"
    let game = mkGame 10 3 3 [1, 1, 1, 1, 1] [0, 1, 0, 1, -1, 3, 0, 3, -2]
    counts <-
      runTestTT
        ( test[
                TestCase(assertEqual "Consume Good Block Success" (1+2) 3)
            ]
        )

    putStrLn "------ Tests Finished! ------"
    if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
