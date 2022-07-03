module Generator where

import qualified Data.Map      as M
import qualified Data.Sequence as S

import           Data.Map      (Map)
import           Data.Sequence (Seq, (|>))
import           System.Random (StdGen, randoms)


{- | Represents type of cell. Each cell can be one of multiple types:
* @Path@  -- Regular cell with no special meaning 
* @Start@ -- Start of maze
* @End@   -- End of maze, finish
-}
data CellType = Path | Start | End deriving (Show)

-- Directions used for navigating through the maze
data Direction = TopD | RightD | BottomD | LeftD deriving (Show)

-- Location in 'Maze'
data Location = Location
    { lx :: Int
    , ly :: Int
    }
    deriving (Show, Eq, Ord)

-- Cell in 'Maze'
data Cell = Cell
    { cellLocation :: Location
    , cellType     :: CellType
    }
    deriving (Show)

-- Maze object
data Maze = Maze
    { mazeWidth  :: Int
    , mazeHeight :: Int
    , mazeCells  :: Map Location Cell
    }
    deriving (Show)


{- | Utility for generating mazes. Contains 'Maze' that is being built, random number
generator and stack with 'Location' to keep track of cells that have not been used yet 
-}
data Generator = Generator
    { gStack :: Seq Location
    , gRng   :: [Int]
    , gMaze  :: Maze
    }
    deriving (Show)

{- | 'generateLabyrinth' @rng w h@ generates labyrinth of width @w@ and height @h@,
utilizing specified random number generator in process.
-}
generateLabyrinth :: StdGen -> Int -> Int -> Maze
generateLabyrinth rng w h = gMaze $ generateBranches $ createInitialPath Generator
    { gStack = fillLocationStack w h
    , gRng = randoms rng
    , gMaze = Maze {mazeWidth = w, mazeHeight = h, mazeCells = M.empty}
    }


{- | 'fillLocationStack' @w h@ creates sequence of 'Location' objects with coordinates
from @(0, 0)@ to @(w - 1, h - 1)@
-}
fillLocationStack :: Int -> Int -> Seq Location
fillLocationStack w h = go 0 0 S.empty
  where
    go :: Int -> Int -> Seq Location -> Seq Location
    go x y stack
        | x == w' && y == h' = newStack
        | x == w'            = go 0 (y + 1) newStack
        | otherwise          = go (x + 1) y newStack
      where
        w' = w - 1
        h' = h - 1
        newStack = stack |> Location x y

{- | 'createInitialPath' @g@ initializes 'Generator' @g@'s 'Maze' with single random path
without any intersections, which can be later expanded using 'generateBranches' function.
Returns new 'Generator' with initialized 'Maze', corresponding stack and new rng
-}
createInitialPath :: Generator -> Generator
createInitialPath g = go g 0 0
  where
    go :: Generator -> Int -> Int -> Generator
    go g x y
        | noPathsAvaliable = g
        | otherwise = error "Not implemented"
      where
        noPathsAvaliable = error "Not implemented"

{- | 'generateBranches' @g m@ fills 'Maze' @m@ using branching. Requires values from
'createInitialPath' to be passed in, otherwise might produce wrong results.
-}
generateBranches :: Generator -> Generator
generateBranches g
    | isStackEmpty = g
    | otherwise = generateBranches g
  where
    isStackEmpty = null $ gStack g

{- | 'getRandomNonCellPath' @g m x y@ tries to pick random avaliable direction from
cell with coordinates @(x, y)@ in 'Maze' @m@. Cannot return direction, which leads to
the other cell. Returns @Nothing@ in case no direction is available; or @Just Direction@,
if succeeded.
-}
getRandomNonCellPath :: Generator -> Int -> Int -> (Generator, Maybe Direction)
getRandomNonCellPath g x y = error "Not implemented"

hasPath :: Maze -> Cell -> Direction -> Bool
hasPath (Maze _ _ cells) (Cell l _) d = M.member (travel l d) cells

{- | 'travel' @loc d@ returns new 'Location', which is result of moving in
'Direction' @d@ from @loc@
-}
travel :: Location -> Direction -> Location
travel (Location x y) d = Location x' y'
  where
    (x', y') = case d of
        TopD    -> (x    , y - 1)
        RightD  -> (x + 1, y    )
        BottomD -> (x    , y + 1)
        LeftD   -> (x - 1, y    )
