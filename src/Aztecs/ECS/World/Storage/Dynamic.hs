{-# LANGUAGE BangPatterns #-}
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
    insertDyn,
    removeDyn,
    removeAny,
    entitiesDyn,
  )
where

import qualified Aztecs.ECS.World.Storage as S
import Control.DeepSeq
import Data.Dynamic
import Data.Maybe

data DynamicStorage = DynamicStorage
  { storageDyn :: !Dynamic,
    insertDyn' :: !(Int -> Dynamic -> Dynamic -> Dynamic),
    removeDyn' :: !(Int -> Dynamic -> (Maybe Dynamic, Dynamic)),
    removeAny' :: !(Int -> Dynamic -> (Maybe DynamicStorage, Dynamic)),
    entitiesDyn' :: !(Dynamic -> [Int]),
    storageRnf :: !(Dynamic -> ())
  }

instance Show DynamicStorage where
  show s = "DynamicStorage " ++ show (storageDyn s)

instance NFData DynamicStorage where
  rnf s = storageRnf s (storageDyn s)

dynStorage :: forall s a. (S.Storage s a) => s a -> DynamicStorage
dynStorage s =
  DynamicStorage
    { storageDyn = toDyn s,
      insertDyn' = \i cDyn sDyn ->
        fromMaybe sDyn $ do
          !s' <- fromDynamic @(s a) sDyn
          !c <- fromDynamic cDyn
          return . toDyn $ S.insert i c s',
      removeDyn' = \i dyn -> case fromDynamic @(s a) dyn of
        Just s' -> let !(a, b) = S.remove i s' in (fmap toDyn a, toDyn b)
        Nothing -> (Nothing, dyn),
      removeAny' = \i dyn -> case fromDynamic @(s a) dyn of
        Just s' -> let !(a, b) = S.remove i s' in (fmap (dynStorage . S.singleton @s i) a, toDyn b)
        Nothing -> (Nothing, dyn),
      entitiesDyn' = \dyn -> case fromDynamic @(s a) dyn of
        Just s' -> map fst $ S.toList s'
        Nothing -> [],
      storageRnf = maybe () rnf . fromDynamic @(s a)
    }

insertDyn :: Int -> Dynamic -> DynamicStorage -> DynamicStorage
insertDyn i c s = s {storageDyn = insertDyn' s i c (storageDyn s)}

removeDyn :: Int -> DynamicStorage -> (Maybe Dynamic, DynamicStorage)
removeDyn i s = let (a, s') = removeDyn' s i (storageDyn s) in (a, s {storageDyn = s'})

removeAny :: Int -> DynamicStorage -> (Maybe DynamicStorage, DynamicStorage)
removeAny i s = let (a, s') = removeAny' s i (storageDyn s) in (a, s {storageDyn = s'})

entitiesDyn :: DynamicStorage -> [Int]
entitiesDyn s = entitiesDyn' s (storageDyn s)
