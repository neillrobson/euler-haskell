{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Singleton2 where

import Data.Kind (Type)
import Data.Singletons.TH (Sing, SingI (sing), SingKind (Demote, fromSing, toSing), SomeSing (SomeSing), genSingletons, withSingI, withSomeSing)

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

$(genSingletons [''DoorState])

-- | Technically, @Door@ is three distinct types, indexed by a type of kind
-- @DoorState@.
data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

--------------------------------------------------------------------------------
-- Existential datatypes
--------------------------------------------------------------------------------

-- | The "Some" in @SomeDoor@ indicates that, if you have a value of this type,
-- you have *either* an opened door, a closed door, or a locked door. It's
-- exactly one of those options.
data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: (SingI s) => Door s -> SomeDoor
fromDoor_ = fromDoor sing

--------------------------------------------------------------------------------
-- Some old definitions
--------------------------------------------------------------------------------

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockAnyDoor :: Sing s -> Door s -> Door 'Locked
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id

--------------------------------------------------------------------------------
-- Interoperability
--------------------------------------------------------------------------------

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
  SOpened -> Just $ fromDoor_ d
  SClosed -> Nothing
  SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor Opened = fromDoor_ . mkDoor SOpened
mkSomeDoor Closed = fromDoor_ . mkDoor SClosed
mkSomeDoor Locked = fromDoor_ . mkDoor SLocked

--------------------------------------------------------------------------------
-- Exercises
--------------------------------------------------------------------------------

data OldSomeDoor :: Type where
  OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor s d) = OldMkSomeDoor (fromSing s) (doorMaterial d)

fromOld :: OldSomeDoor -> SomeDoor
fromOld (OldMkSomeDoor ds m) = case toSing ds of
  SomeSing s -> MkSomeDoor s (UnsafeMkDoor m)

fromOld' :: OldSomeDoor -> SomeDoor
fromOld' (OldMkSomeDoor ds m) = withSomeSing ds $ \s -> MkSomeDoor s (UnsafeMkDoor m)

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
  | n `mod` 2 == 1 = Just $ UnsafeMkDoor m
  | otherwise = Nothing

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n d = case unlockDoor n d of
  Just d' -> fromDoor_ d'
  Nothing -> fromDoor_ d

openAnyDoor :: (SingI s) => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ SOpened = Just
    openAnyDoor_ SClosed = Just . openDoor
    openAnyDoor_ SLocked = fmap openDoor . unlockDoor n

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n (MkSomeDoor s d) = case withSingI s (openAnyDoor n d) of
  Just d' -> fromDoor_ d'
  Nothing -> withSingI s fromDoor_ d

data List a = Nil | Cons a (List a)

data SList :: List a -> Type where
  SNil :: SList 'Nil
  SCons :: Sing x -> SList xs -> SList ('Cons x xs)

type instance Sing = SList

instance (SingKind k) => SingKind (List k) where
  type Demote (List k) = List (Demote k)

  fromSing :: Sing (xs :: List k) -> List (Demote k)
  fromSing = \case
    SNil -> Nil
    SCons sx sxs -> Cons (fromSing sx) (fromSing sxs)

  toSing :: List (Demote k) -> SomeSing (List k)
  toSing = \case
    Nil -> SomeSing SNil
    Cons x xs -> withSomeSing x $ \sx -> SomeSing $ SCons sx (toSing xs)
