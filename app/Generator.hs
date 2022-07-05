module Generator (generateMaze) where

import qualified Data.Map            as M

import           System.Random       (StdGen)

import           Generator.Branching (growBranches)
import           Generator.Core      (Generator (gMaze), Maze (Maze))
import           Generator.Init      (initGenerator)

{- | 'generateMaze' @rng w h@ generates maze of width @w@ and height @h@,
utilizing specified random number generator in process.
-}
generateMaze :: StdGen -> Int -> Int -> Maze
generateMaze rng w h = gMaze $ growBranches $ initGenerator rng w h
