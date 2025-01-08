{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.Storage
  ( Storage (..),
    Table (..),
    table,
    ComponentStorage (..),
  )
where

import Data.Aztecs.Core (Entity)
import Data.Data (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)

class (Typeable s) => Storage s a where
  insert :: Entity -> a -> s a -> (s a -> a, s a)
  lookup :: Entity -> s a -> Maybe a
  toList :: s a -> [(Entity, s a -> a)]

newtype Table c = Table (Vector (Entity, c))

table :: ComponentStorage a c
table = ComponentStorage $ Table (V.empty)

instance Storage Table c where
  insert e c (Table t) =
    let t' = V.cons (e, c) t
        idx = V.length t' - 1
     in ((\(Table newT) -> snd $ newT V.! idx), Table (t'))
  lookup e (Table t) = snd <$> V.find (\(e', _) -> e' == e) t
  toList (Table t) = map (\((e, _), idx) -> (e, \(Table newT) -> snd $ newT V.! idx)) (zip (V.toList t) [0 ..])

data ComponentStorage a c where
  ComponentStorage :: (Storage s c) => s c -> ComponentStorage a c
