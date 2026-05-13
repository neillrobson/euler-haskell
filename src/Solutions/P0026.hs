module Solutions.P0026 where

import Data.List (elemIndex)

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

recurringDigitsFor :: Integer -> Int
recurringDigitsFor n = go []
  where
    go [] = go [(0, 1)]
    go ss@(s : _)
      | r == 0 = 0
      | otherwise = rep
      where
        next@(_, r) = nextDigit n s
        mbRep = next `elemIndex` ss
        rep = case mbRep of
          Just i -> i + 1
          Nothing -> go $ next : ss

type Remainder = Integer

type Digit = Integer

type State = (Digit, Remainder)

nextDigit :: Integer -> State -> State
nextDigit _ s@(_, 0) = s
nextDigit d (_, n) = (x, r)
  where
    (x, r) = (n * 10) `divMod` d
