module Generator (generateMaze) where

import qualified Data.Map            as M

import           Data.List
import           System.Random       (StdGen, randoms)

import           Generator.Branching (generateBranches)
import           Generator.Core      (Generator (..), Location (Location), Maze (..))
import           Generator.Initial   (createInitialPath, fillLocationStack)
import           Generator.Util      (addCell)

{- | 'generateLabyrinth' @rng w h@ generates labyrinth of width @w@ and height @h@,
utilizing specified random number generator in process.
-}
generateMaze :: StdGen -> Int -> Int -> Maze
generateMaze rng w h = gMaze $ generateBranches $ createInitialPath
    Generator
    { gStack = fillLocationStack w h
    , gRng = randoms rng
    , gMaze = addCell (Location 0 0) Maze { mazeWidth = w, mazeHeight = h, mazeCells = M.empty }
    }
