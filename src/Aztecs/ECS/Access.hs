{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Access (MonadAccess(..), AccessT(..), runAccessT) where

import Aztecs.ECS
import Aztecs.ECS.Access.Class
import Aztecs.ECS.Entities.Class
import Aztecs.ECS.Query
import Aztecs.ECS.System.Class
import Control.Monad.Primitive
import Control.Monad.State
import Data.SparseSet hiding (insert, lookup)
import qualified Data.SparseSet as S
import Data.Word
import Prelude hiding (lookup)

newtype AccessT c m a = AccessT {unAccessT :: StateT (SparseSet Word32 c) m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance {-# OVERLAPPING #-} (Monad m) => MonadAccess c (AccessT c m) where
  insert e = AccessT . modify . S.insert (entityIndex e)
  lookup e = AccessT $ do
    s <- get
    return $ S.lookup s (entityIndex e)

instance (MonadAccess c2 m) => MonadAccess c2 (AccessT c m) where
  insert e = lift . insert e
  lookup = lift . lookup

instance {-# OVERLAPPING #-} (Monad m) => MonadSystem c (AccessT c m) where
  query = Query . AccessT $ S.toList <$> get
  {-# INLINE query #-}

instance (MonadSystem c2 m) => MonadSystem c2 (AccessT c m) where
  query = Query . lift $ unQuery query
  {-# INLINE query #-}

instance (MonadEntities m) => MonadEntities (AccessT c m) where
  spawn = lift spawn
  entities = Query . lift $ unQuery entities

instance (PrimMonad m) => PrimMonad (AccessT c m) where
  type PrimState (AccessT c m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

runAccessT :: (Monad m) => AccessT c m a -> SparseSet Word32 c -> m (a, SparseSet Word32 c)
runAccessT (AccessT m) = runStateT m
{-# INLINE runAccessT #-}
