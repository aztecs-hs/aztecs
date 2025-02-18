{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Dynamic.Reader
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
  { dynQueryReaderAll :: !([i] -> [EntityID] -> Archetype -> m [o]),
    dynQueryReaderLookup :: !(i -> EntityID -> Archetype -> m (Maybe o))
  }

instance (Functor m) => Functor (DynamicQueryReader m i) where
  fmap f q =
    DynamicQueryReader
      { dynQueryReaderAll =
          \i es arch -> fmap (fmap f) $ dynQueryReaderAll q i es arch,
        dynQueryReaderLookup = \i eId arch -> fmap (fmap f) $ dynQueryReaderLookup q i eId arch
      }

instance (Monad m) => Applicative (DynamicQueryReader m i) where
  pure a =
    DynamicQueryReader
      { dynQueryReaderAll = \_ es _ -> pure (take (length es) $ repeat a),
        dynQueryReaderLookup = \_ _ _ -> pure $ Just a
      }
  f <*> g =
    DynamicQueryReader
      { dynQueryReaderAll = \i es arch -> do
          as <- dynQueryReaderAll g i es arch
          fs <- dynQueryReaderAll f i es arch
          return (zipWith ($) fs as),
        dynQueryReaderLookup = \i eId arch -> do
          res <- dynQueryReaderLookup g i eId arch
          case res of
            Just a -> do
              res' <- dynQueryReaderLookup f i eId arch
              return (fmap ($) res' <*> Just a)
            Nothing -> pure Nothing
      }

instance (Monad m) => Category (DynamicQueryReader m) where
  id =
    DynamicQueryReader
      { dynQueryReaderAll = \as _ _ -> pure as,
        dynQueryReaderLookup = \a _ _ -> pure $ Just a
      }
  f . g =
    DynamicQueryReader
      { dynQueryReaderAll = \i es arch -> do
          as <- dynQueryReaderAll g i es arch
          dynQueryReaderAll f as es arch,
        dynQueryReaderLookup = \i eId arch -> do
          res <- dynQueryReaderLookup g i eId arch
          case res of
            Just a -> dynQueryReaderLookup f a eId arch
            Nothing -> pure Nothing
      }

instance (Monad m) => Arrow (DynamicQueryReader m) where
  arr f =
    DynamicQueryReader
      { dynQueryReaderAll = \bs _ _ -> pure $ fmap f bs,
        dynQueryReaderLookup = \b _ _ -> pure . Just $ f b
      }
  first f =
    DynamicQueryReader
      { dynQueryReaderAll = \bds es arch -> do
          let (bs, ds) = unzip bds
          cs <- dynQueryReaderAll f bs es arch
          return $ zip cs ds,
        dynQueryReaderLookup = \(b, d) eId arch -> do
          res <- dynQueryReaderLookup f b eId arch
          return $ case res of
            Just c -> Just (c, d)
            Nothing -> Nothing
      }

instance (Monad m) => ArrowDynamicQueryReader (DynamicQueryReader m) where
  entityDyn =
    DynamicQueryReader
      { dynQueryReaderAll = \_ es _ -> pure es,
        dynQueryReaderLookup = \_ eId _ -> pure $ Just eId
      }
  fetchDyn cId =
    DynamicQueryReader
      { dynQueryReaderAll = \_ _ arch -> let !as = A.all cId arch in pure $ fmap snd as,
        dynQueryReaderLookup = \_ eId arch -> pure $ A.lookupComponent eId cId arch
      }
  fetchMaybeDyn cId =
    DynamicQueryReader
      { dynQueryReaderAll = \_ _ arch -> let as = A.allMaybe cId arch in pure $ fmap snd as,
        dynQueryReaderLookup = \_ eId arch -> pure $ Just <$> A.lookupComponent eId cId arch
      }
