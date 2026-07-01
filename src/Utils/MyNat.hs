{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Utils.MyNat where

import Data.Kind (Type)
import Numeric.Natural (Natural)

-- | Type synonym for 'Natural'. The goal is to recreate type-level natural
-- numbers with singletons.
type Nat = Natural

-- | The singleton for `Nat`. Clearly, not all the cases have been enumerated,
-- nor could they ever feasibly be enumerated in this way. This is just for
-- demonstration.
data SNat :: Nat -> Type where
  SZero :: SNat 0
  SOne :: SNat 1
  STwo :: SNat 2
  SThree :: SNat 3
