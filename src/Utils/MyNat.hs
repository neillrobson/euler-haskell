{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.MyNat where

import Data.Kind (Type)
import GHC.Num (integerToNatural)
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

instance KnownNat 0 where
  natSing = SZero

instance KnownNat 1 where
  natSing = SOne

instance KnownNat 2 where
  natSing = STwo

instance KnownNat 3 where
  natSing = SThree

-- | The proxy value carries the witness to the fact that 'n' is a 'KnownNat'.
-- We cannot pass 'natSing' directly to 'fromSNat' without a witness to the fact
-- that 'natSing' exists for the given value of 'n'. The proxy, being this
-- function's only argument, is that witness.
natVal :: (KnownNat n) => proxy n -> Natural
natVal = fromProxy natSing

-- | Same as 'natVal' but inlined. The 'go' function needs the proxy argument
-- only because it's tied to the 'KnownNat' witness at the top. Removing the
-- proxy, 'go' would not know what 'SNat' to resolve 'natSing' to.
natVal' :: (KnownNat n) => proxy n -> Natural
natVal' = go natSing
  where
    go :: SNat m -> proxy m -> Natural
    go SZero _ = 0
    go SOne _ = 1
    go STwo _ = 2
    go SThree _ = 3

-- | A third, more direct form using ScopedTypeVariables.
natVal'' :: forall n proxy. (KnownNat n) => proxy n -> Natural
natVal'' _ = case natSing :: SNat n of
  SZero -> 0
  SOne -> 1
  STwo -> 2
  SThree -> 3

--------------------------------------------------------------------------------

newtype Mod (m :: Nat) = Mod {unMod :: Natural} deriving (Eq, Ord)

instance (KnownNat m) => Num (Mod m) where
  mx@(Mod x) * Mod y = Mod $ x * y `mod` natVal mx
  mx@(Mod x) + Mod y = Mod $ (x + y) `mod` natVal mx
  negate mx@(Mod x) = Mod $ if x == 0 then 0 else natVal mx - x
  abs = id
  signum (Mod x) = Mod $ signum x
  fromInteger x = mx
    where
      mx = Mod $ (integerToNatural x) `mod` natVal mx

instance (KnownNat m) => Show (Mod m) where
  show mx@(Mod x) = show x ++ "  (mod " ++ show (natVal mx) ++ ")"

type ModThree = Mod 3

twoModThree :: ModThree
twoModThree = Mod 2

shouldBeOne :: ModThree
shouldBeOne = twoModThree + twoModThree
