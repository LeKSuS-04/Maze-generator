module Main where

import           System.Random (getStdGen)
import           Generator     (generateMaze)

main :: IO ()
main = do
    rng <- getStdGen
    let maze = generateMaze rng 50 50
    print maze
