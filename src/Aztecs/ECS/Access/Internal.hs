{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.ECS.Access.Internal
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Access.Internal
  ( Access (..),
    runAccessWith,
    evalAccess,
    triggerEvent,
    triggerEntityEvent,
  )
where

import Aztecs.ECS.Entity
import Aztecs.ECS.Event
import Aztecs.ECS.World.Internal
import qualified Aztecs.ECS.World.Observers as O
import Aztecs.ECS.World.Observers.Internal
import Control.Monad
import Control.Monad.Fix
import Control.Monad.State
import Data.Dynamic
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable

-- | Access into a `World`.
newtype Access m a = Access {unAccess :: StateT (World m) m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance MonadTrans Access where
  lift = Access . lift

-- | Run an `Access` with a given `World`, returning the result and updated world.
runAccessWith :: Access m a -> World m -> m (a, World m)
runAccessWith a = runStateT (unAccess a)

-- | Run an `Access` with a given `World`, returning only the result.
evalAccess :: (Monad m) => Access m a -> World m -> m a
evalAccess a = evalStateT (unAccess a)

-- | Trigger an event.
triggerEvent :: forall m e. (Monad m, Event e) => e -> Access m ()
triggerEvent evt = Access $ do
  !w <- get
  let eventTypeRep = typeOf (Proxy @e)
      globalOs = O.lookupGlobalObservers eventTypeRep $ observers w
      callbacks = mapMaybe (\oId -> O.lookupCallback oId (observers w)) $ Set.toList globalOs
      dynEvt = toDyn evt
  forM_ callbacks $ \cb -> case cb of
    DynEventObserver f -> f dynEvt
    DynEntityObserver _ -> pure ()

-- | Trigger an event for a specific entity.
triggerEntityEvent ::
  forall m e.
  (Monad m, Event e) =>
  EntityID ->
  e ->
  Access m ()
triggerEntityEvent targetEntity evt = Access $ do
  !w <- get
  let eventTypeRep = typeOf $ Proxy @e
      entityOs = O.lookupEntityObservers eventTypeRep targetEntity $ observers w
      callbacks = mapMaybe (\oId -> O.lookupCallback oId (observers w)) $ Set.toList entityOs
      dynEvt = toDyn evt
  forM_ callbacks $ \cb -> case cb of
    DynEntityObserver f -> f targetEntity dynEvt
    DynEventObserver _ -> pure ()
