module Solutions.P0073 where

import Data.Ratio (Ratio, denominator, numerator, (%))

solve :: Integer
solve = fareyCountBtwn 12000 (1 % 3) (1 % 2)

--------------------------------------------------------------------------------

type Farey = Ratio Integer

fareyCountBtwn :: Integer -> Farey -> Farey -> Integer
fareyCountBtwn limit minim maxim = go limit minim maxim (0 % 1) (1 % limit) 0
  where
    go :: Integer -> Farey -> Farey -> Farey -> Farey -> Integer -> Integer
    go lim mn mx ab cd count
      | cd >= mx = count
      | otherwise = go lim mn mx cd pq count'
      where
        count' = if cd > mn then count + 1 else count
        a = numerator ab
        b = denominator ab
        c = numerator cd
        d = denominator cd
        k = (lim + b) `div` d
        p = k * c - a
        q = k * d - b
        pq = p % q
