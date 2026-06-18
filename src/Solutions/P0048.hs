module Solutions.P0048 where

import Utils.Modular (Mod (Mod, unMod))
import Utils.Powers (bExp)

solve :: Integer
solve = unMod $ selfPowerSum 1000

--------------------------------------------------------------------------------

type ModTenTen = Mod 10000000000

selfPowerSum :: Integer -> ModTenTen
selfPowerSum x = sum $ map (\i -> bExp (Mod i) i) [1 .. x]
