module Generator.Core where

import           Data.Map          (Map)
import           Data.Sequence     (Seq)

{- | Represents type of cell. Each cell can be one of multiple types:
* @Path@  -- Regular cell with no special meaning
* @Start@ -- Start of maze
* @End@   -- End of maze, finish
-}
data CellType = Path | Start | End deriving (Show)

-- Directions used for navigating through the maze
data Direction = TopD | RightD | BottomD | LeftD deriving (Show, Eq)

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
    , cellPaths    :: [Direction]
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