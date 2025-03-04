{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
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
  ( Bundle (..),
    MonoidBundle (..),
    MonoidDynamicBundle (..),
    runBundle,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype
import Aztecs.ECS.World.Bundle.Class
import Aztecs.ECS.World.Bundle.Dynamic
import Aztecs.ECS.World.Components
import qualified Aztecs.ECS.World.Components as CS
import Data.Set (Set)
import qualified Data.Set as Set

-- | Bundle of components.
--
-- @since 9.0
newtype Bundle = Bundle
  { -- | Unwrap the bundle.
    --
    -- @since 9.0
    unBundle :: Components -> (Set ComponentID, Components, DynamicBundle)
  }

-- | @since 9.0
instance Monoid Bundle where
  mempty = Bundle (Set.empty,,mempty)

-- | @since 9.0
instance Semigroup Bundle where
  Bundle b1 <> Bundle b2 = Bundle $ \cs ->
    let (cIds1, cs', d1) = b1 cs
        (cIds2, cs'', d2) = b2 cs'
     in (cIds1 <> cIds2, cs'', d1 <> d2)

-- | @since 9.0
instance MonoidBundle Bundle where
  bundle :: forall a. (Component a) => a -> Bundle
  bundle a = Bundle $ \cs ->
    let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', dynBundle cId a)

-- | @since 9.0
instance MonoidDynamicBundle Bundle where
  dynBundle cId c = Bundle (Set.singleton cId,,dynBundle cId c)

-- | Insert a bundle of components into an archetype.
--
-- @since 9.0
runBundle :: Bundle -> Components -> EntityID -> Archetype -> (Components, Archetype)
runBundle b cs eId arch =
  let !(_, cs', d) = unBundle b cs
      !arch' = runDynamicBundle d eId arch
   in (cs', arch')
