module Solutions.P0097 where

import Solutions.P0048 (ModTenTen, modularExp)
import Utils.Modular (unMod)

solve :: Integer
solve = unMod $ powerOfTwo * 28433 + 1

--------------------------------------------------------------------------------

powerOfTwo :: ModTenTen
powerOfTwo = modularExp 2 7830457
