{-# LANGUAGE ViewPatterns #-}

module Solutions.P0025 where

solve :: Integer
solve = firstFibWithDigitCount 1000

findFibWithDigitCount :: Integer -> Integer
findFibWithDigitCount n = go 0 (1, 1)
  where
    go i (a, b) = if n <= digitCount a then i else go (i + 1) (b, a + b)

digitCount :: Integer -> Integer
digitCount = go 1
  where
    go i x = if x > 10 then go (i + 1) (x `div` 10) else i

--------------------------------------------------------------------------------

-- Direct computation using Binet's formula

goldenRatio :: Double
goldenRatio = (1 + sqrt 5) / 2

firstFibWithDigitCount :: Integer -> Integer
firstFibWithDigitCount (fromIntegral -> n) = ceiling $ (n - 1 + (logBase 10 5 / 2)) / logBase 10 goldenRatio
