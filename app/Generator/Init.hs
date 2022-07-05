module Generator.Init (initGenerator) where

import qualified Data.Map         as M

import           Data.Map         (Map)
import           Data.Maybe       (isNothing)
import           System.Random    (StdGen, randoms)

import           Generator.Core   (Generator (..), Location (Location), Maze (..))
import           Generator.Random (chooseRandom)
import           Generator.Util   (addCell, getFreeDirections, updateCell)

{- | 'initGenerator' @rng w h@ initializes 'Generator' with 'Maze' of width @w@ and
height @h@. Also creates 'Cell' at 'Location' @(0, 0)@ that acts as a starting cell
and is used to grow branches. Adds this 'Location' to the stack of 'Generator'.
-}
initGenerator :: StdGen -> Int -> Int -> Generator
initGenerator rng w h =
    Generator
    { gStack = [startLocation]
    , gRng = randoms rng
    , gMaze = addCell startLocation Maze { mazeWidth = w, mazeHeight = h, mazeCells = M.empty }
    }
  where
    startLocation = Location 0 0
