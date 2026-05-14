module Solutions.P0028 where

solve :: Integer
solve = undefined

{-
A scanning sum:
1
2 2 2 2
4 4 4 4
6 6 6 6

 1
 3  5  7  9
13 17 21 25
31 37 43 49

1
10 * 2 = 20; 20 + 4 * 1 = 24; 24 + 1 = 25
10 * 4 = 40; 40 + 4 * 9 = 76; 76 + 25 = 101
-}

-- Without the initial 1^2 = 1
firstNOddSquares :: Integer -> Integer
firstNOddSquares n = 4 * n' ^ 2 + (11 * n' + 4 * n' ^ 3) `div` 3
  where
    n' = n - 1
