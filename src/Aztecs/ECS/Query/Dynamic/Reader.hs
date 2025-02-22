{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Dynamic.Reader
  ( -- * Dynamic queries
    DynamicQueryReader (..),
    ArrowDynamicQueryReader (..),

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Component
import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import qualified Aztecs.ECS.World.Storage as S
import Control.Arrow
import Control.Category
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Set (Set)

-- | Dynamic query for components by ID.
newtype DynamicQueryReader i o
  = DynamicQueryReader {dynQueryReaderAll :: [i] -> [EntityID] -> Archetype -> [o]}
  deriving (Functor)

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

instance ArrowChoice DynamicQueryReader where
  left f = DynamicQueryReader $ \eds es arch ->
    let (es', ds) = partitionEithers eds
        cs = dynQueryReaderAll f es' es arch
     in fmap Left cs ++ fmap Right ds

instance ArrowDynamicQueryReader DynamicQueryReader where
  entity = DynamicQueryReader $ \_ es _ -> es
  fetchDyn cId = DynamicQueryReader $ \_ _ arch ->
    let !as = fromMaybe [] $ S.toList <$> A.lookupStorage cId arch in fmap snd as
  fetchMaybeDyn cId = DynamicQueryReader $ \_ es arch -> case A.lookupStorage cId arch of
    Just s -> let !as = S.toList s in fmap Just $ snd <$> as
    Nothing -> map (const Nothing) es

data DynamicQueryFilter = DynamicQueryFilter
  { filterWith :: !(Set ComponentID),
    filterWithout :: !(Set ComponentID)
  }

instance Semigroup DynamicQueryFilter where
  DynamicQueryFilter withA withoutA <> DynamicQueryFilter withB withoutB =
    DynamicQueryFilter (withA <> withB) (withoutA <> withoutB)

instance Monoid DynamicQueryFilter where
  mempty = DynamicQueryFilter mempty mempty
