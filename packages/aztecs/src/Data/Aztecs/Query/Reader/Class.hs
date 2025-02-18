{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Reader.Class (ArrowQueryReader (..)) where

import Control.Arrow (Arrow (..))
import Data.Aztecs.Component
import Data.Aztecs.Entity (EntityID)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

class (Arrow arr) => ArrowQueryReader arr where
  -- | Fetch the currently matched `EntityID`.
  entity :: arr () EntityID

  -- | Fetch a `Component` by its type.
  fetch :: (Component a) => arr () a

  -- | Fetch a `Component` by its type, returning `Nothing` if it doesn't exist.
  fetchMaybe :: (Component a) => arr () (Maybe a)
