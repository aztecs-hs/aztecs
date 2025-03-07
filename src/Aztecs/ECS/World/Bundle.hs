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
    bundle,
    runBundle,
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity
import Aztecs.ECS.World.Archetype
import Aztecs.ECS.World.Bundle.Dynamic
import Aztecs.ECS.World.Components
import qualified Aztecs.ECS.World.Components as CS
import Data.Set (Set)
import qualified Data.Set as Set

-- | Bundle of components.
--
-- @since 0.9
newtype Bundle = Bundle
  { -- | Unwrap the bundle.
    --
    -- @since 0.9
    unBundle :: Components -> (Set ComponentID, Components, DynamicBundle)
  }

-- | @since 0.9
instance Monoid Bundle where
  mempty = Bundle (Set.empty,,mempty)

-- | @since 0.9
instance Semigroup Bundle where
  Bundle b1 <> Bundle b2 = Bundle $ \cs ->
    let (cIds1, cs', d1) = b1 cs
        (cIds2, cs'', d2) = b2 cs'
     in (cIds1 <> cIds2, cs'', d1 <> d2)

-- | @since 0.11
bundle :: forall a. (Component a) => a -> Bundle
bundle a = Bundle $ \cs ->
  let (cId, cs') = CS.insert @a cs in (Set.singleton cId, cs', dynBundle cId a)

-- | Insert a bundle of components into an archetype.
--
-- @since 0.9
runBundle :: Bundle -> Components -> EntityID -> Archetype -> (Components, Archetype)
runBundle b cs eId arch =
  let !(_, cs', d) = unBundle b cs
      !arch' = runDynamicBundle d eId arch
   in (cs', arch')
