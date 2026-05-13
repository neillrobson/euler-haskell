module Solutions.P0026 where

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

recurringDigitsFor :: Integer -> [Integer]
recurringDigitsFor n = undefined

type Remainder = Integer

type Digit = Integer

type State = (Digit, Remainder)

nextDigit :: Integer -> State -> State
nextDigit _ s@(_, 0) = s
nextDigit d (_, n) = (x, r')
  where
    (x, r) = n `divMod` d
    r' = if r == n then n * 10 else r
