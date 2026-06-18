module Solutions.P0097 where

import Solutions.P0048 (ModTenTen)
import Utils.Modular (unMod)
import Utils.Powers (bExp)

solve :: Integer
solve = unMod $ powerOfTwo * 28433 + 1

--------------------------------------------------------------------------------

powerOfTwo :: ModTenTen
powerOfTwo = bExp 2 (7830457 :: Integer)
