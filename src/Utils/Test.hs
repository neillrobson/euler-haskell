{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Test where

class Mul a b c where
  (.*.) :: a -> b -> c

instance Mul Int Int Int where (.*.) = (*)

instance Mul Int Float Float where x .*. y = fromIntegral x * y

instance (Mul a b c) => Mul a [b] [c] where x .*. v = map (x .*.) v

-- f = \z x y -> if z then x .*. [y] else y

{-
With fundep:

x .*. [y] requires Mul a [b] r.
The "else y" further requires r ~ b, so we match Mul a [b] b.

We're going to match with Mul a [b] [c].
The fundep says that the third param is deterministic,
so b must unify with [c].

Now we "just" need to match the constraint, Mul a [c] c. Loop.

Without fundep:
-}

g :: Bool -> Int -> [[Float]] -> [[Float]]
g z x y = if z then x .*. y else y

{-
f = \z x y ->
  if   z
  then x .*. [y]
  else y

Resolve Mul a [b] [c]:
x         :: a
[y]       :: [b]
x .*. [y] :: [c]
y         :: [c]
...which leaves Mul a [[c]] [c].

Constraint becomes Mul a [c] c.

Change variable names for clarity:
instance (Mul d e f) => Mul d [e] [f]

Resolve Mul d [e] [f]:
a         ~ d
[c]       ~ [e]
c         ~ [f]
...which leaves Mul d [[f]] [f].

Back where we started.
-}
