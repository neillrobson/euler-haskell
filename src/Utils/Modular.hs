{-# LANGUAGE KindSignatures #-}

module Utils.Modular where

import GHC.TypeLits (KnownNat, Nat, natVal)

newtype Mod (m :: Nat) = Mod {unMod :: Integer} deriving (Eq, Ord)

instance (KnownNat m) => Num (Mod m) where
  mx@(Mod x) * Mod y = Mod $ x * y `mod` natVal mx
  mx@(Mod x) + Mod y = Mod $ if xy > m then xy - m else xy
    where
      xy = x + y
      m = natVal mx
  negate mx@(Mod x) = Mod $ if x == 0 then 0 else natVal mx - x
  abs = id
  signum (Mod x) = Mod $ signum x
  fromInteger x = mx
    where
      mx = Mod $ x `mod` natVal mx

instance (KnownNat m) => Show (Mod m) where
  show mx@(Mod x) = show x ++ "  (mod " ++ show (natVal mx) ++ ")"
