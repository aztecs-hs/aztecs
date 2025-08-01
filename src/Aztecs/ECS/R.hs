{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.ECS.R where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query (Query (..))
import Aztecs.ECS.Queryable
import Control.Monad.Primitive
import qualified Data.SparseSet.Strict.Mutable as MS
import Prelude hiding (Read, lookup)

newtype R a = R {unR :: a}
  deriving (Show, Eq, Functor)

instance (PrimMonad m, Functor m) => Queryable m (R a) where
  type QueryableAccess (R a) = '[Read a]
  queryable (HCons s HEmpty) _ = Query $ do
    !as <- MS.toList s
    let go (_, c) = R c
    return $ map (fmap go) as
