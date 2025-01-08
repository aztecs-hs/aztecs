{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.Storage
  ( Storage (..),
    Table (..),
    table,
    ComponentStorage (..),
    lookupComponent,
    insertComponent,
  )
where

import Data.Aztecs.Core (Entity)
import Data.Data (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)

class (Typeable s) => Storage s a where
  insert :: Entity -> a -> s a -> s a
  lookup :: Entity -> s a -> Maybe a

newtype Table c = Table (Vector (Entity, c))

table :: ComponentStorage c
table = ComponentStorage $ Table (V.empty)

instance Storage Table c where
  insert e c (Table t) = Table (V.cons (e, c) t)
  lookup e (Table t) = snd <$> V.find (\(e', _) -> e' == e) t

data ComponentStorage c where
  ComponentStorage :: (Storage s c) => s c -> ComponentStorage c

insertComponent :: Entity -> c -> ComponentStorage c -> ComponentStorage c
insertComponent e c (ComponentStorage s) = ComponentStorage (insert e c s)

lookupComponent :: Entity -> ComponentStorage c -> Maybe c
lookupComponent e (ComponentStorage s) = lookup e s
