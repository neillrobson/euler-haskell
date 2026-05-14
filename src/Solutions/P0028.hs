module Solutions.P0028 where

solve :: Integer
solve = spiralDiagNLayers 500

-- Without the initial 1^2 = 1.
-- Zero-indexed.
firstNOddSquares :: Integer -> Integer
firstNOddSquares n = 4 * n ^ (2 :: Int) + (11 * n + 4 * n ^ (3 :: Int)) `div` 3

spiralDiagNLayers :: Integer -> Integer
spiralDiagNLayers n = 4 * firstNOddSquares n - 6 * n * (n + 1) + 1
