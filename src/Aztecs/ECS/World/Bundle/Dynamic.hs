{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Aztecs.ECS.World.Bundle.Dynamic
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Bundle.Dynamic (DynamicBundleT (..), DynamicBundle, MonoidDynamicBundle (..)) where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype
import Aztecs.ECS.World.Bundle.Dynamic.Class
import Control.Monad.Identity

-- | Dynamic bundle of components.
data DynamicBundleT m = DynamicBundleT
  { -- | Insert components into an archetype.
    runDynamicBundle :: EntityID -> Archetype m -> (Archetype m, Access m ()),
    -- | Run the on-insert hooks for all components.
    runOnInsert :: Access m ()
  }

-- | Pure dynamic bundle.
type DynamicBundle = DynamicBundleT Identity

instance (Monad m) => Semigroup (DynamicBundleT m) where
  DynamicBundleT d1 h1 <> DynamicBundleT d2 h2 =
    DynamicBundleT
      ( \eId arch ->
          let (arch', hook1) = d1 eId arch
              (arch'', hook2) = d2 eId arch'
           in (arch'', hook1 >> hook2)
      )
      (h1 >> h2)

instance (Monad m) => Monoid (DynamicBundleT m) where
  mempty = DynamicBundleT (\_ arch -> (arch, return ())) (return ())

instance (Monad m) => MonoidDynamicBundle m (DynamicBundleT m) where
  dynBundle cId a = DynamicBundleT (\eId arch -> insertComponent eId cId a arch) (return ())
