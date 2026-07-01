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

fromSNat :: SNat n -> Natural
fromSNat SZero = 0
fromSNat SOne = 1
fromSNat STwo = 2
fromSNat SThree = 3

fromProxy :: SNat n -> proxy n -> Natural
fromProxy snat _ = fromSNat snat

class KnownNat (n :: Nat) where
  natSing :: SNat n

natVal :: (KnownNat n) => proxy n -> Natural
natVal = fromProxy natSing
