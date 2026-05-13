module Solutions.P0026 where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

recurringDigitsFor :: Integer -> Int
recurringDigitsFor n = go []
  where
    go [] = go [(0, 10)]
    go ss@(s : _) = if r == 0 then 0 else fromMaybe (go $ next : ss) rep
      where
        next@(_, r) = nextDigit n s
        rep = (`div` 2) . (+ 1) <$> elemIndex next ss

type Remainder = Integer

type Digit = Integer

type State = (Digit, Remainder)

nextDigit :: Integer -> State -> State
nextDigit _ s@(_, 0) = s
nextDigit d (_, n) = (x, r')
  where
    (x, r) = n `divMod` d
    r' = if r == n then n * 10 else r
