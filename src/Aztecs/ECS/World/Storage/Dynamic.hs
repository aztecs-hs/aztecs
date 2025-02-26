{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

data DynamicStorage = DynamicStorage
  { storageDyn :: !Dynamic,
    singletonDyn' :: !(Dynamic -> Dynamic),
    toAscListDyn' :: !(Dynamic -> [Dynamic]),
    fromAscListDyn' :: !([Dynamic] -> Dynamic),
    storageRnf :: !(Dynamic -> ())
  }

instance Show DynamicStorage where
  show s = "DynamicStorage " ++ show (storageDyn s)

instance NFData DynamicStorage where
  rnf s = storageRnf s (storageDyn s)

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

singletonDyn :: Dynamic -> DynamicStorage -> DynamicStorage
singletonDyn dyn s = s {storageDyn = singletonDyn' s dyn}

fromAscListDyn :: [Dynamic] -> DynamicStorage -> DynamicStorage
fromAscListDyn dyns s = s {storageDyn = fromAscListDyn' s dyns}

toAscListDyn :: DynamicStorage -> [Dynamic]
toAscListDyn = toAscListDyn' <*> storageDyn
