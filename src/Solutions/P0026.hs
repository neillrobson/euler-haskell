module Solutions.P0026 where

solve :: Integer
solve = undefined

--------------------------------------------------------------------------------

recurringDigitsFor :: Integer -> [Integer]
recurringDigitsFor n = undefined

-- Next: also store remainder with each digit, to identify loops
type State = ([Integer], Integer)

nextDigit :: Integer -> State -> State
nextDigit _ s@(_, 0) = s
nextDigit d (xs, r) = (x : xs, if r == r' then r * 10 else r')
  where
    (x, r') = r `divMod` d
