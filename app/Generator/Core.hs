{-# LANGUAGE InstanceSigs #-}
module Generator.Core where

import qualified Data.Map   as M

import           Data.Array (listArray, (!))
import           Data.Map   (Map)

{- | Represents type of cell. Each cell can be one of multiple types:
* @Path@  -- Regular cell with no special meaning
* @Start@ -- Start of maze
* @End@   -- End of maze, finish
-}
data CellType = Path | Start | End deriving (Show)

-- Directions used for navigating through the maze
data Direction = TopD | RightD | BottomD | LeftD deriving (Eq)

-- | 'oppositeDirectoin' @d@ gives direction, opposite to direction @d@
oppositeDirection :: Direction -> Direction
oppositeDirection d = case d of
    TopD    -> BottomD
    RightD  -> LeftD
    BottomD -> TopD
    LeftD   -> RightD

-- Location in 'Maze'
data Location = Location
    { lx :: Int
    , ly :: Int
    }
    deriving (Eq, Ord)

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

-- Cell in 'Maze'
data Cell = Cell
    { cellLocation :: Location
    , cellType     :: CellType
    , cellPaths    :: [Direction]
    }

-- Maze object
data Maze = Maze
    { mazeWidth  :: Int
    , mazeHeight :: Int
    , mazeCells  :: Map Location Cell
    }

instance Show Maze where
    show :: Maze -> String
    show m@Maze{mazeWidth=w, mazeHeight=h, mazeCells=cs} =
        unlines [[showWallCell j i | j <- [0..w]]
                                   | i <- [0..h]]
      where
        showWallCell :: Int -> Int -> Char
        showWallCell x y = wallMapping !
            ( pathDoesNotExist (x-1) (y-1)  x    (y-1) 1
            + pathDoesNotExist  x    (y-1)  x     y    2
            + pathDoesNotExist  x     y    (x-1)  y    4
            + pathDoesNotExist (x-1)  y    (x-1) (y-1) 8
            )
          where
            wallMapping = listArray (0, 15) " ╵╶└╷│┌├╴┘─┴┐┤┬┼"

            pathDoesNotExist :: Int -> Int ->  Int -> Int -> Int -> Int
            pathDoesNotExist x y x' y' v = case ( M.lookup (Location x  y ) cs
                                                , M.lookup (Location x' y') cs) of
                (Nothing, Nothing) -> 0
                (_, Nothing)       -> v
                (Nothing, _)       -> v
                (Just c@Cell{cellLocation=l, cellPaths=ds}, Just c'@Cell{cellLocation=l'}) ->
                    if any (\d -> travel l d == l') ds then 0 else v

{- | Utility for generating mazes. Contains 'Maze' that is being built, random number
generator and stack with 'Location's to ensure no empty space exist in final maze
-}
data Generator = Generator
    { gStack :: [Location]
    , gRng   :: [Int]
    , gMaze  :: Maze
    }
