{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.R (R (..)) where

import Aztecs.ECS.HSet (HSetT (..), Lookup (..))
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.System
import Aztecs.Internal
import qualified Aztecs.World as W
import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.SparseSet.Strict.Mutable as MS
import Prelude hiding (Read, lookup)

-- | Read-only 'Queryable' component access.
newtype R a = R {unR :: a}
  deriving (Show, Eq, Functor)

instance (PrimMonad m, Lookup a cs) => Queryable (AztecsT cs m) (R a) where
  type QueryableAccess (R a) = '[Read a]
  queryable = AztecsT $ do
    w <- get
    !as <- lift $ MS.toList . lookup $ W.worldComponents w
    let go (_, c) = R c
    return . Query $ map (fmap go) as
  {-# INLINE queryable #-}
