{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- * Dynamic query filters
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

-- | Dynamic query for components by ID.
newtype DynamicQueryReader i o
  = DynamicQueryReader {dynQueryReaderAll :: [i] -> [EntityID] -> Archetype -> [o]}

instance Functor (DynamicQueryReader i) where
  fmap f q = DynamicQueryReader $ \i es arch -> f <$> dynQueryReaderAll q i es arch

instance Applicative (DynamicQueryReader i) where
  pure a = DynamicQueryReader $ \_ es _ -> replicate (length es) a

  f <*> g =
    DynamicQueryReader $ \i es arch ->
      let as = dynQueryReaderAll g i es arch
          fs = dynQueryReaderAll f i es arch
       in zipWith ($) fs as

instance Category DynamicQueryReader where
  id = DynamicQueryReader $ \as _ _ -> as
  f . g = DynamicQueryReader $ \i es arch ->
    let as = dynQueryReaderAll g i es arch in dynQueryReaderAll f as es arch

instance Arrow DynamicQueryReader where
  arr f = DynamicQueryReader $ \bs _ _ -> fmap f bs
  first f = DynamicQueryReader $ \bds es arch ->
    let (bs, ds) = unzip bds
        cs = dynQueryReaderAll f bs es arch
     in zip cs ds

instance ArrowDynamicQueryReader DynamicQueryReader where
  entityDyn = DynamicQueryReader $ \_ es _ -> es
  fetchDyn cId =
    DynamicQueryReader $ \_ _ arch -> let !as = A.all cId arch in fmap snd as
  fetchMaybeDyn cId =
    DynamicQueryReader $ \_ _ arch -> let as = A.allMaybe cId arch in fmap snd as

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty
