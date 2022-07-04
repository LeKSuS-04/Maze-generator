module Main where

import           System.Random (getStdGen)
import           Generator     (generateLabyrinth)

main :: IO ()
main = do
    rng <- getStdGen
    let labyrinth = generateLabyrinth rng 50 50
    print labyrinth
