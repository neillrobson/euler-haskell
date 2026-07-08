{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.MyNat where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Num (integerToNatural)
import qualified GHC.TypeNats as TN
import Numeric.Natural (Natural)

-- | Type synonym for 'Natural'. The goal is to recreate type-level natural
-- numbers with singletons.
type Nat = Natural

--------------------------------------------------------------------------------
-- Nat singleton, explicit enumeration
--------------------------------------------------------------------------------

-- | The singleton for `Nat`. Clearly, not all the cases have been enumerated,
-- nor could they ever feasibly be enumerated in this way. This is just for
-- demonstration.
data ENat :: Nat -> Type where
  SZero :: ENat 0
  SOne :: ENat 1
  STwo :: ENat 2
  SThree :: ENat 3

fromENat :: ENat n -> Natural
fromENat SZero = 0
fromENat SOne = 1
fromENat STwo = 2
fromENat SThree = 3

fromProxy :: ENat n -> proxy n -> Natural
fromProxy snat _ = fromENat snat

class KnownENat (n :: Nat) where
  natESing :: ENat n

instance KnownENat 0 where
  natESing = SZero

instance KnownENat 1 where
  natESing = SOne

instance KnownENat 2 where
  natESing = STwo

instance KnownENat 3 where
  natESing = SThree

--------------------------------------------------------------------------------
-- Nat singleton, tacit enumeration
--------------------------------------------------------------------------------

newtype SNat (n :: Nat) = UnsafeSNat Natural

fromSNat :: SNat n -> Natural
fromSNat (UnsafeSNat n) = n

fromSProxy :: SNat n -> proxy n -> Natural
fromSProxy snat _ = fromSNat snat

class KnownNat (n :: Nat) where
  natSing :: SNat n

instance (TN.KnownNat n) => KnownNat n where
  natSing = UnsafeSNat $ TN.natVal $ Proxy @n

--------------------------------------------------------------------------------
-- natVal: retrieving the Natural value from a type
--------------------------------------------------------------------------------

-- | The proxy value carries the witness to the fact that 'n' is a 'KnownNat'.
-- We cannot pass 'natSing' directly to 'fromSNat' without a witness to the fact
-- that 'natSing' exists for the given value of 'n'. The proxy, being this
-- function's only argument, is that witness.
natVal :: (KnownENat n) => proxy n -> Natural
natVal = fromProxy natESing

-- | Same as 'natVal' but inlined. The 'go' function needs the proxy argument
-- only because it's tied to the 'KnownNat' witness at the top. Removing the
-- proxy, 'go' would not know what 'SNat' to resolve 'natSing' to.
natVal' :: (KnownENat n) => proxy n -> Natural
natVal' = go natESing
  where
    go :: ENat m -> proxy m -> Natural
    go SZero _ = 0
    go SOne _ = 1
    go STwo _ = 2
    go SThree _ = 3

-- | A third, more direct form using ScopedTypeVariables.
natVal'' :: forall n proxy. (KnownENat n) => proxy n -> Natural
natVal'' _ = case natESing :: ENat n of
  SZero -> 0
  SOne -> 1
  STwo -> 2
  SThree -> 3

natVal_ :: forall n proxy. (KnownNat n) => proxy n -> Natural
natVal_ _ = case natSing :: SNat n of
  UnsafeSNat n -> n

--------------------------------------------------------------------------------

newtype Mod (m :: Nat) = Mod {unMod :: Natural} deriving (Eq, Ord)

instance (KnownNat m) => Num (Mod m) where
  mx@(Mod x) * Mod y = Mod $ x * y `mod` natVal_ mx
  mx@(Mod x) + Mod y = Mod $ (x + y) `mod` natVal_ mx
  negate mx@(Mod x) = Mod $ if x == 0 then 0 else natVal_ mx - x
  abs = id
  signum (Mod x) = Mod $ signum x
  fromInteger x = mx
    where
      mx = Mod $ (integerToNatural x) `mod` natVal_ mx

instance (KnownNat m) => Show (Mod m) where
  show mx@(Mod x) = show x ++ "  (mod " ++ show (natVal_ mx) ++ ")"

type ModThree = Mod 3

twoModThree :: ModThree
twoModThree = Mod 2

shouldBeOne :: ModThree
shouldBeOne = twoModThree + twoModThree
