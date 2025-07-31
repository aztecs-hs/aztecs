{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.ECS.R where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Control.Monad.Primitive
import qualified Data.SparseSet.Strict.Mutable as MS
import Prelude hiding (Read, lookup)

newtype R a = R {unR :: a}
  deriving (Show, Eq, Functor)

instance (PrimMonad m, PrimState m ~ s) => QueryItem s m (R a) where
  type QueryItemAccess (R a) = '[Read a]
  queryable (HCons s HEmpty) _ = Query $ do
    !as <- MS.toList s
    let go (_, c) = R c
    return $ fmap (fmap go) as
