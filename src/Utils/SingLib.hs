{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.SingLib where

import Data.Singletons.TH

data DoorState = Opened | Closed | Locked deriving (Eq, Show)

$(genSingletons [''DoorState])

newtype Door (s :: DoorState) = UnsafeMkDoor {doorMaterial :: String}

--------------------------------------------------------------------------------
-- Basic utilities using unsafe door construction.
--------------------------------------------------------------------------------

-- | Opens a closed door.
openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

-- | Unlocks a door, but only if the user enters an odd number as a password.
unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor i (UnsafeMkDoor m)
  | odd i = Just $ UnsafeMkDoor m
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Generic utilities using singletons.
--------------------------------------------------------------------------------

-- | Coerces any @Door@ to an @Opened@ state.
openAnyDoor :: (SingI s) => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor i = go sing
  where
    -- This type signature is necessary here to tie together the parameter for
    -- @Sing@ and @Door@. The name @t@ is only used to distinguish it from @s@
    -- in the type signature above for @openAnyDoor@.
    go :: Sing t -> Door t -> Maybe (Door 'Opened)
    go SOpened = Just
    go SClosed = Just . openDoor
    go SLocked = fmap openDoor . unlockDoor i
