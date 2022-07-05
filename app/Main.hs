module Main where

import           System.Random (getStdGen)
import           Generator     (generateMaze)

main :: IO ()
main = do
    rng <- getStdGen
    let maze = generateMaze rng 60 30
    print maze
