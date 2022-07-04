module Generator.Branching (generateBranches) where

import           Generator.Core

{- | 'generateBranches' @g@ fills 'Maze' in 'Generator' @g@ using branching. Requires
'Generator' to be initialized by 'createInitialPath', otherwise might produce
wrong results.
-}
generateBranches :: Generator -> Generator
generateBranches g
    | isStackEmpty = g
    | otherwise = error "Not implemented"
  where
    isStackEmpty = null $ gStack g
