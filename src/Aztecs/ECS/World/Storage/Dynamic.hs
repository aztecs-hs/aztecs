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
import Control.DeepSeq
import Data.Dynamic
import Data.Maybe

-- | Dynamic storage of components.
--
-- @since 9.0
data DynamicStorage = DynamicStorage
  { -- | Dynamic storage.
    --
    -- @since 9.0
    storageDyn :: !Dynamic,
    -- | Singleton storage.
    --
    -- @since 9.0
    singletonDyn' :: !(Dynamic -> Dynamic),
    -- | Convert this storage to an ascending list.
    --
    -- @since 9.0
    toAscListDyn' :: !(Dynamic -> [Dynamic]),
    -- | Convert from an ascending list.
    --
    -- @since 9.0
    fromAscListDyn' :: !([Dynamic] -> Dynamic),
    -- | Reduce this storage to normal form.
    --
    -- @since 9.0
    storageRnf :: !(Dynamic -> ())
  }

-- | @since 9.0
instance Show DynamicStorage where
  show s = "DynamicStorage " ++ show (storageDyn s)

-- | @since 9.0
instance NFData DynamicStorage where
  rnf s = storageRnf s (storageDyn s)

-- | Create a dynamic storage from a storage.
--
-- @since 9.0
{-# INLINE dynStorage #-}
dynStorage :: forall a s. (S.Storage a s) => s -> DynamicStorage
dynStorage s =
  DynamicStorage
    { storageDyn = toDyn s,
      singletonDyn' = toDyn . S.singleton @a @s . fromMaybe (error "TODO") . fromDynamic,
      toAscListDyn' = \d -> map toDyn (S.toAscList @a @s (fromMaybe (error "TODO") $ fromDynamic d)),
      fromAscListDyn' = toDyn . S.fromAscList @a @s . map (fromMaybe (error "TODO") . fromDynamic),
      storageRnf = maybe () rnf . fromDynamic @s
    }

-- | Singleton dynamic storage.
--
-- @since 9.0
singletonDyn :: Dynamic -> DynamicStorage -> DynamicStorage
singletonDyn dyn s = s {storageDyn = singletonDyn' s dyn}

-- | Convert from an ascending list.
--
-- @since 9.0
fromAscListDyn :: [Dynamic] -> DynamicStorage -> DynamicStorage
fromAscListDyn dyns s = s {storageDyn = fromAscListDyn' s dyns}

-- | Convert this storage to an ascending list.
--
-- @since 9.0
toAscListDyn :: DynamicStorage -> [Dynamic]
toAscListDyn = toAscListDyn' <*> storageDyn
