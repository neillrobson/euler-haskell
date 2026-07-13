{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Test where

class Mul a b c | a b -> c where
  (.*.) :: a -> b -> c

instance Mul Int Int Int where (.*.) = (*)

instance Mul Int Float Float where x .*. y = fromIntegral x * y

instance (Mul d e e) => Mul d [e] [e] where x .*. v = map (x .*.) v

f = \z x y -> if z then x .*. [y] else x

{-
x .*. [y] requires Mul a [b] c.
The "else y" further requires c ~ b, so we match Mul a [b] b.

Without a fundep, there is nothing else we can do with `a` and `b`:
they are ambiguous, so we do not commit to any instance. We stop.

With a fundep `d e -> f`, the third type argument can be eagerly solved for,
because it is guaranteed unique for any choice of the first two.

So when considering `instance Mul d [e] [f]`, `b` can be eagerly replaced with
`[f]` and the implications considered.

d ~ a, e ~ b, and b ~~ [f], so the constraint can become `Mul a [f] f`.
Loop ensues.

--------------------------------------------------------------------------------

The coverage condition prevents this sort of unterminated loop.

For every type variable "determined" on the right hand side, its presence on the
LHS provides a binding to the target type signature.

Say the instance was:

instance (Mul d e e) => Mul d [e] [e]

Attempt the same typecheck for Mul a [b] a.
d ~ a, e ~ b, a ~~ [e]: This time, we can use the earlier unifications to
resolve that final forcing, creating a ~~ [b].

Using the replacements in the constraint, Mul d e e becomes Mul [b] b b.
Still one ambiguous type variable, but no recursion!
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
