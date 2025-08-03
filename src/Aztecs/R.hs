{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.R (R (..)) where

import Aztecs.ECS.Access.Internal
import qualified Aztecs.ECS.Access.Internal as A
import Aztecs.ECS.Class
import Aztecs.ECS.Executor
import Aztecs.ECS.HSet (HSet (..), Lookup (..))
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.Schedule
import Aztecs.ECS.Scheduler
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.ECS.System
import Aztecs.Entities
import qualified Aztecs.Entities as E
import Aztecs.Internal
import Aztecs.World (ComponentStorage, bundle)
import qualified Aztecs.World as W
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)

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
