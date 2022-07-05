module Generator.Util where

import qualified Data.Map       as M

import           Generator.Core

{- | 'getNonWallDirections' @l m@ returns list of 'Direction's from 'Location' @l@ in
'Maze' @m@ that do not lead into the wall (out of bounds)
-}
getNonWallDirections :: Location -> Maze -> [Direction]
getNonWallDirections (Location x y) Maze{mazeWidth=w, mazeHeight=h} =
    (if y /= 0     then (TopD:)    else id) $
    (if x /= w - 1 then (RightD:)  else id) $
    (if y /= h - 1 then (BottomD:) else id) $
    (if x /= 0     then (LeftD:)   else id) []

{- | 'getFreeDirections' @l @m returns list of 'Direction' from 'Location' @l@ in
'Maze' @m@ that do not lead into the wall (out of bounds) or other existing cell.
Might return empty list, if no such route exists.
-}
getFreeDirections :: Location -> Maze -> [Direction]
getFreeDirections l m@Maze{mazeCells=cs} = [ d | d <- getNonWallDirections l m,
                                                 M.notMember (travel l d) cs ]

{- | 'addCell' @l m@ adds cell to 'Location' @l@ in the 'Maze' @m@. Automatically
determines it's type by location:
* @(0, 0)@ -- 'Start'
* @(w-1, h-1)@ -- 'End'
* any other -- 'Path'
-}
addCell :: Location -> Maze -> Maze
addCell l@Location{lx=x, ly=y} m@Maze{mazeWidth=w, mazeHeight=h, mazeCells=cs} =
    m { mazeCells = M.insert l (Cell l t []) cs }
  where
    t | x == 0 && y == 0         = Start
      | x == w - 1 && y == h - 1 = End
      | otherwise                = Path

{- | 'updateCell' @l d m@ updates cell in 'Maze' @m@  at 'Location' @l@ to have
'Direction' @d@. Also updates cell adjacent to it to make complete path. For example, if
'Cell' @A@ is being updates with 'Direction' 'RightD' in this example:

@
   |   |   |
---+---+---+---
   | A | A'|
---+---+---+---
   |   |   |
@

...then cell @A'@ will also be updated, with 'Direction' 'LeftD'. That means that
following is true for any cell:

@
updateCell l d m = updateCell (travel l d) (oppositeDirection d) m
@

Doesn't do anything if no cell exists in specified location
-}
updateCell :: Location -> Direction -> Maze -> Maze
updateCell l d m = updateSingleCell l' d' $ updateSingleCell l d m
  where
    l' = travel l d
    d' = oppositeDirection d

    updateSingleCell :: Location -> Direction -> Maze -> Maze
    updateSingleCell l d m@Maze{mazeCells=cs} = m { mazeCells = M.adjust (addDirection d) l cs }
      where
        addDirection :: Direction -> Cell -> Cell
        addDirection d c@Cell{cellPaths=ds} = c { cellPaths = d : ds }
