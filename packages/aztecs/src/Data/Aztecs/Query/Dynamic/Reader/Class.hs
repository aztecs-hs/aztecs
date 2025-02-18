{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..)) where

import Control.Arrow (Arrow (..))
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

class (Arrow arr) => ArrowDynamicQueryReader arr where
  -- | Fetch the `EntityID` belonging to this entity.
  entityDyn :: arr () EntityID

  -- | Fetch a `Component` by its `ComponentID`.
  fetchDyn :: (Component a) => ComponentID -> arr () a

  -- | Try to fetch a `Component` by its `ComponentID`.
  fetchMaybeDyn :: (Component a) => ComponentID -> arr () (Maybe a)
