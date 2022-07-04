module Generator.Initial (fillLocationStack, createInitialPath) where

import qualified Data.Map         as M
import qualified Data.Sequence    as S

import           Data.Map         (Map)
import           Data.Maybe       (isNothing)
import           Data.Sequence    (Seq, (|>))
import           System.Random    (StdGen, randoms)

import           Generator.Core
import           Generator.Random (chooseRandom)
import           Generator.Util   (addCell, getFreeDirections, oppositeDirection, travel,
                                   updateCell)

{- | 'fillLocationStack' @w h@ creates sequence of 'Location' objects with coordinates
from @(0, 0)@ to @(w - 1, h - 1)@
-}
fillLocationStack :: Int -> Int -> Seq Location
fillLocationStack w h = go 0 0 S.empty
  where
    go :: Int -> Int -> Seq Location -> Seq Location
    go x y stack
        | x == w - 1 && y == h - 1 = newStack
        | x == w - 1               = go 0 (y + 1) newStack
        | otherwise                = go (x + 1) y newStack
      where
        newStack = stack |> Location x y

{- | 'createInitialPath' @g@ initializes 'Generator' @g@'s 'Maze' with single random path
without any intersections, which can be later expanded using 'generateBranches' function.
Returns new 'Generator' with initialized 'Maze', corresponding stack and new rng
-}
createInitialPath :: Generator -> Generator
createInitialPath g = go g $ Location 0 0
  where
    go :: Generator -> Location -> Generator
    go g l = case nextDirection of
        Nothing -> g'
        Just d  -> go g' { gMaze = updateCell l d $ addCell (travel l d) m' } $ travel l d
      where
        (g'@Generator {gMaze=m'}, nextDirection) = getRandomNonCellDirection g l


{- | 'getRandomNonCellDirection' @g l@ tries to pick random avaliable direction from
cell with location @l@ in 'Maze' from 'Generator' @g@. Cannot return direction that
leads to the other cell. Returns @Nothing@ in case no direction is available; otherwise
@Just Direction@.
-}
getRandomNonCellDirection :: Generator -> Location -> (Generator, Maybe Direction)
getRandomNonCellDirection g@Generator{gMaze=m} l = chooseRandom g $ getFreeDirections l m
