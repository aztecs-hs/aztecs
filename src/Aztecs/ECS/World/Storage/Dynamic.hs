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
    fromAscListDyn,
    toAscListDyn,
  )
where

import qualified Aztecs.ECS.World.Storage as S
import Data.Dynamic
import Data.Maybe

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
    -- | Convert this storage to an ascending list.
    --
    -- @since 0.9
    toAscListDyn' :: !(Dynamic -> [Dynamic]),
    -- | Convert from an ascending list.
    --
    -- @since 0.9
    fromAscListDyn' :: !([Dynamic] -> Dynamic)
  }

-- | @since 0.9
instance Show DynamicStorage where
  show s = "DynamicStorage " ++ show (storageDyn s)

-- | Create a dynamic storage from a storage.
--
-- @since 0.9
{-# INLINE dynStorage #-}
dynStorage :: forall a s. (S.Storage a s) => s -> DynamicStorage
dynStorage s =
  DynamicStorage
    { storageDyn = toDyn s,
      singletonDyn' = toDyn . S.singleton @a @s . fromMaybe (error "TODO") . fromDynamic,
      toAscListDyn' = \d -> map toDyn (S.toAscList @a @s (fromMaybe (error "TODO") $ fromDynamic d)),
      fromAscListDyn' = toDyn . S.fromAscList @a @s . map (fromMaybe (error "TODO") . fromDynamic)
    }

-- | Singleton dynamic storage.
--
-- @since 0.9
singletonDyn :: Dynamic -> DynamicStorage -> DynamicStorage
singletonDyn dyn s = s {storageDyn = singletonDyn' s dyn}

-- | Convert from an ascending list.
--
-- @since 0.9
fromAscListDyn :: [Dynamic] -> DynamicStorage -> DynamicStorage
fromAscListDyn dyns s = s {storageDyn = fromAscListDyn' s dyns}

-- | Convert this storage to an ascending list.
--
-- @since 0.9
toAscListDyn :: DynamicStorage -> [Dynamic]
toAscListDyn = toAscListDyn' <*> storageDyn
