{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.World.Bundle
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Bundle
  ( BundleT (..),
    Bundle,
    MonoidDynamicBundle (..),
    bundle,
    bundleUntracked,
    runBundle,
  )
where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype
import Aztecs.ECS.World.Bundle.Dynamic
import Aztecs.ECS.World.Components
import qualified Aztecs.ECS.World.Components as CS
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set

-- | Bundle of components.
newtype BundleT m = BundleT
  { -- | Unwrap the bundle.
    unBundle :: Components -> (Set ComponentID, Components, DynamicBundle m)
  }

-- | Pure bundle of components.
type Bundle = BundleT Identity

instance (Monad m) => Monoid (BundleT m) where
  mempty = BundleT (Set.empty,,mempty)

instance (Monad m) => Semigroup (BundleT m) where
  BundleT b1 <> BundleT b2 = BundleT $ \cs ->
    let (cIds1, cs', d1) = b1 cs
        (cIds2, cs'', d2) = b2 cs'
     in (cIds1 <> cIds2, cs'', d1 <> d2)

bundle :: forall m a. (Component m a) => a -> BundleT m
bundle a = BundleT $ \cs ->
  let (cId, cs') = CS.insert @a @m cs in (Set.singleton cId, cs', dynBundle @m cId a)

-- | Create a bundle that inserts without running lifecycle hooks.
bundleUntracked :: forall m a. (Component m a) => a -> BundleT m
bundleUntracked a = BundleT $ \cs ->
  let (cId, cs') = CS.insert @a @m cs in (Set.singleton cId, cs', dynBundleUntracked @m cId a)

instance (Monad m) => MonoidDynamicBundle m (BundleT m) where
  dynBundle cId c = BundleT (Set.singleton cId,,dynBundle @m cId c)
  dynBundleUntracked cId c = BundleT (Set.singleton cId,,dynBundleUntracked @m cId c)

-- | Insert a bundle of components into an archetype.
runBundle :: (Monad m) => BundleT m -> Components -> EntityID -> Archetype m -> (Components, Archetype m, Access m ())
runBundle b cs eId arch =
  let !(_, cs', d) = unBundle b cs
      !(arch', hook) = runDynamicBundle d eId arch
   in (cs', arch', hook)
