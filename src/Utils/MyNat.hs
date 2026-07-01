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

natVal :: (KnownNat n) => proxy n -> Integer
natVal = toInteger . fromProxy natSing

--------------------------------------------------------------------------------

newtype Mod (m :: Nat) = Mod {unMod :: Integer} deriving (Eq, Ord)

instance (KnownNat m) => Num (Mod m) where
  mx@(Mod x) * Mod y = Mod $ x * y `mod` natVal mx
  mx@(Mod x) + Mod y = Mod $ x + y `mod` natVal mx
  negate mx@(Mod x) = Mod $ if x == 0 then 0 else natVal mx - x
  abs = id
  signum (Mod x) = Mod $ signum x
  fromInteger x = mx
    where
      mx = Mod $ x `mod` natVal mx

instance (KnownNat m) => Show (Mod m) where
  show mx@(Mod x) = show x ++ "  (mod " ++ show (natVal mx) ++ ")"

type ModThree = Mod 3

type ModSeven = Mod 7

eightModSeven :: ModSeven
eightModSeven = Mod 8

shouldBeTwo :: ModSeven
shouldBeTwo = eightModSeven + eightModSeven

eightModThree :: ModThree
eightModThree = Mod 8

shouldBeFour :: ModThree
shouldBeFour = eightModThree + eightModThree
