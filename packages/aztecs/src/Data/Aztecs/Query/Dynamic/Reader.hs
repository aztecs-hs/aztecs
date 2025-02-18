{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    DynamicQueryFilter (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Data.Set (Set)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty

-- | Dynamic query for components by ID.
data DynamicQueryReader m i o = DynamicQueryReader
  { dynQueryAll :: !([i] -> [EntityID] -> Archetype -> m [o]),
    dynQueryLookup :: !(i -> EntityID -> Archetype -> m (Maybe o))
  }

instance (Functor m) => Functor (DynamicQueryReader m i) where
  fmap f q =
    DynamicQueryReader
      { dynQueryAll =
          \i es arch -> fmap (fmap f) $ dynQueryAll q i es arch,
        dynQueryLookup = \i eId arch -> fmap (fmap f) $ dynQueryLookup q i eId arch
      }

instance (Monad m) => Applicative (DynamicQueryReader m i) where
  pure a =
    DynamicQueryReader
      { dynQueryAll = \_ es _ -> pure (take (length es) $ repeat a),
        dynQueryLookup = \_ _ _ -> pure $ Just a
      }
  f <*> g =
    DynamicQueryReader
      { dynQueryAll = \i es arch -> do
          as <- dynQueryAll g i es arch
          fs <- dynQueryAll f i es arch
          return (zipWith ($) fs as),
        dynQueryLookup = \i eId arch -> do
          res <- dynQueryLookup g i eId arch
          case res of
            Just a -> do
              res' <- dynQueryLookup f i eId arch
              return (fmap ($) res' <*> Just a)
            Nothing -> pure Nothing
      }

instance (Monad m) => Category (DynamicQueryReader m) where
  id =
    DynamicQueryReader
      { dynQueryAll = \as _ _ -> pure as,
        dynQueryLookup = \a _ _ -> pure $ Just a
      }
  f . g =
    DynamicQueryReader
      { dynQueryAll = \i es arch -> do
          as <- dynQueryAll g i es arch
          dynQueryAll f as es arch,
        dynQueryLookup = \i eId arch -> do
          res <- dynQueryLookup g i eId arch
          case res of
            Just a -> dynQueryLookup f a eId arch
            Nothing -> pure Nothing
      }

instance (Monad m) => Arrow (DynamicQueryReader m) where
  arr f =
    DynamicQueryReader
      { dynQueryAll = \bs _ _ -> pure $ fmap f bs,
        dynQueryLookup = \b _ _ -> pure . Just $ f b
      }
  first f =
    DynamicQueryReader
      { dynQueryAll = \bds es arch -> do
          let (bs, ds) = unzip bds
          cs <- dynQueryAll f bs es arch
          return $ zip cs ds,
        dynQueryLookup = \(b, d) eId arch -> do
          res <- dynQueryLookup f b eId arch
          return $ case res of
            Just c -> Just (c, d)
            Nothing -> Nothing
      }

instance (Monad m) => ArrowDynamicQueryReader (DynamicQueryReader m) where
  entityDyn =
    DynamicQueryReader
      { dynQueryAll = \_ es _ -> pure es,
        dynQueryLookup = \_ eId _ -> pure $ Just eId
      }
  fetchDyn cId =
    DynamicQueryReader
      { dynQueryAll = \_ _ arch -> let !as = A.all cId arch in pure $ fmap snd as,
        dynQueryLookup = \_ eId arch -> pure $ A.lookupComponent eId cId arch
      }
  fetchMaybeDyn cId =
    DynamicQueryReader
      { dynQueryAll = \_ _ arch -> let as = A.allMaybe cId arch in pure $ fmap snd as,
        dynQueryLookup = \_ eId arch -> pure $ Just <$> A.lookupComponent eId cId arch
      }
