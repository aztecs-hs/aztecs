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
module Aztecs.ECS.World.Bundle.Dynamic (DynamicBundle (..), MonoidDynamicBundle (..)) where

import Aztecs.ECS.Access.Internal (Access)
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype
import Aztecs.ECS.World.Bundle.Dynamic.Class

-- | Dynamic bundle of components.
newtype DynamicBundle m = DynamicBundle
  { -- | Insert components into an archetype.
    runDynamicBundle :: EntityID -> Archetype m -> (Archetype m, Access m ())
  }

instance (Monad m) => Semigroup (DynamicBundle m) where
  DynamicBundle d1 <> DynamicBundle d2 = DynamicBundle go
    where
      go eId arch =
        let (arch', hook1) = d1 eId arch
            (arch'', hook2) = d2 eId arch'
         in (arch'', hook1 >> hook2)

instance (Monad m) => Monoid (DynamicBundle m) where
  mempty = DynamicBundle (\_ arch -> (arch, return ()))

instance (Monad m) => MonoidDynamicBundle m (DynamicBundle m) where
  dynBundle cId a = DynamicBundle (\eId arch -> insertComponent eId cId a arch)
  dynBundleUntracked cId a = DynamicBundle (\eId arch -> (insertComponentUntracked eId cId a arch, return ()))
