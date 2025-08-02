{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.ECS.Queryable.R where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query (Query (..))
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Control.Monad.Primitive
import qualified Data.SparseSet.Strict.Mutable as MS
import Prelude hiding (Read, lookup)

newtype R a = R {unR :: a}
  deriving (Show, Eq, Functor)

instance (PrimMonad m, Lookup a cs) => Queryable cs m (R a) where
  type QueryableAccess (R a) = '[Read a]
  queryable cs _ = Query $ do
    !as <- MS.toList $ lookup cs
    let go (_, c) = R c
    return $ map (fmap go) as
  {-# INLINE queryable #-}
