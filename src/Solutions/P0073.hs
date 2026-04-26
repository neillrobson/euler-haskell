module Solutions.P0073 where

import Data.Ratio ((%))

solve :: Integer
solve = fromIntegral $ ratiosBetweenThirdAndHalf 12000

--------------------------------------------------------------------------------

type Farey = (Int, Int)

fareySequence :: Int -> [Farey]
fareySequence limit = (0, 1) : (1, limit) : next (fareySequence limit)
  where
    -- First two to satisfy exhaustive pattern-match
    next [] = []
    next [_] = []
    -- Base case: we reached (1, 1)
    next (_ : (_, 1) : _) = []
    -- General case
    next ((a, b) : rest@((c, d) : _)) = (p, q) : next rest
      where
        k = (limit + b) `div` d
        p = k * c - a
        q = k * d - b

ratiosBetweenThirdAndHalf :: Int -> Int
ratiosBetweenThirdAndHalf =
  length
    . takeWhile (\(a, b) -> (a % b) < (1 % 2))
    . dropWhile (\(a, b) -> (a % b) <= (1 % 3))
    . fareySequence
