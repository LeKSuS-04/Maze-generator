module Generator.Branching (growBranches) where

import           Data.Maybe       (isNothing)

import           Generator.Core   (Direction, Generator (..), Location)
import           Generator.Random (chooseRandom)
import           Generator.Util   (addCell, getFreeDirections, travel, updateCell)

{- | 'growBranches' @g@ /"grows"/ 'Maze' inside 'Generator' @g@ using branching
technique. It works by taking top element from @stack@ and growing it in random
directions until there's no place to grow. Since each cell is connected with at
least one other cell (in fact, at least two, but one is already enough), by the
end of process all cells of 'Maze' will be in use. It will not work only if the
'Generator' @g@ wasn't configured properly before being passed in this function.
-}
growBranches :: Generator -> Generator
growBranches g@(Generator [] _ _) = g
growBranches g@(Generator (l:ls) rng m) =
    case newDirection of
        Nothing -> growBranches g' { gStack = ls }
        Just d  -> let new = travel l d
                   in growBranches g' { gStack = new:l:ls
                                      , gMaze = updateCell l d $ addCell new m
                                      }
  where
    (g'@Generator {gMaze=m'}, newDirection) = getRandomNonCellDirection g l


{- | 'getRandomNonCellDirection' @g l@ tries to pick random avaliable direction from
cell with location @l@ in 'Maze' from 'Generator' @g@. Cannot return direction that
leads to the other cell. Returns @Nothing@ in case no direction is available; otherwise
@Just Direction@.
-}
getRandomNonCellDirection :: Generator -> Location -> (Generator, Maybe Direction)
getRandomNonCellDirection g@Generator{gMaze=m} l = chooseRandom g $ getFreeDirections l m
