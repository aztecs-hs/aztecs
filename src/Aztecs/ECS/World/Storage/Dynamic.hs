{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.World.Storage.Dynamic
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.World.Storage.Dynamic
  ( DynamicStorage (..),
    dynStorage,
    singletonDyn,
    fromAscVectorDyn,
    toAscVectorDyn,
  )
where

import qualified Aztecs.ECS.World.Storage as S
import Data.Dynamic
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Dynamic storage of components.
--
-- @since 0.9
data DynamicStorage = DynamicStorage
  { -- | Dynamic storage.
    --
    -- @since 0.9
    storageDyn :: !Dynamic,
    -- | Singleton storage.
    --
    -- @since 0.9
    singletonDyn' :: !(Dynamic -> Dynamic),
    -- | Convert this storage to an ascending vector.
    --
    -- @since 0.9
    toAscVectorDyn' :: !(Dynamic -> Vector Dynamic),
    -- | Convert from an ascending vector.
    --
    -- @since 0.9
    fromAscVectorDyn' :: !(Vector Dynamic -> Dynamic)
  }

instance Show DynamicStorage where
  show s = "DynamicStorage " ++ show (storageDyn s)

-- | Create a dynamic storage from a storage.
--
-- @since 0.9
dynStorage :: forall a s. (S.Storage a s) => s -> DynamicStorage
dynStorage s =
  DynamicStorage
    { storageDyn = toDyn s,
      singletonDyn' = toDyn . S.singleton @a @s . fromMaybe (error "TODO") . fromDynamic,
      toAscVectorDyn' = \d -> V.map toDyn (S.toAscVector @a @s (fromMaybe (error "TODO") $ fromDynamic d)),
      fromAscVectorDyn' = toDyn . S.fromAscVector @a @s . V.map (fromMaybe (error "TODO") . fromDynamic)
    }
{-# INLINE dynStorage #-}

-- | Singleton dynamic storage.
--
-- @since 0.9
singletonDyn :: Dynamic -> DynamicStorage -> DynamicStorage
singletonDyn dyn s = s {storageDyn = singletonDyn' s dyn}

-- | Convert from an ascending vector.
--
-- @since 0.9
fromAscVectorDyn :: Vector Dynamic -> DynamicStorage -> DynamicStorage
fromAscVectorDyn dyns s = s {storageDyn = fromAscVectorDyn' s dyns}

-- | Convert this storage to an ascending vector.
--
-- @since 0.9
toAscVectorDyn :: DynamicStorage -> Vector Dynamic
toAscVectorDyn = toAscVectorDyn' <*> storageDyn
