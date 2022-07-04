module Generator.Random (chooseRandom) where

import           Generator.Core (Generator (Generator, gRng))


{- | 'chooseRandom' @g as@ chooses random element from list of @as@ and returns it and
new generator.

Returns 'Nothing' and unchanged 'Generator' for empty lists
-}
chooseRandom :: Generator -> [a] -> (Generator, Maybe a)
chooseRandom g [] = (g , Nothing)
chooseRandom g as = (g', Just $ as !! (n `mod` l))
  where
    (g', n) = getRandom g
    l = length as

{- | 'getRandom' @g@ retrieves random integer from 'Generator' @g@ and returns it with
new 'Generator'. -}
getRandom :: Generator -> (Generator, Int)
getRandom g@Generator{gRng=[]}     = error "Empty generator"  -- Shouldn't happen
getRandom g@Generator{gRng=(r:rs)} = (g { gRng = rs }, r)
