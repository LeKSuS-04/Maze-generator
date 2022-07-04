module Generator (generateMaze) where

import qualified Data.Map            as M

import           Data.List
import           System.Random       (StdGen, randoms)

import           Generator.Branching (growBranches)
import           Generator.Core      (Generator (..), Location (Location), Maze (..))
import           Generator.Init      (initGenerator)
import           Generator.Util      (addCell)

{- | 'generateMaze' @rng w h@ generates maze of width @w@ and height @h@,
utilizing specified random number generator in process.
-}
generateMaze :: StdGen -> Int -> Int -> Maze
generateMaze rng w h = gMaze $ growBranches $ initGenerator rng w h
