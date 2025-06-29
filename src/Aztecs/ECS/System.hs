{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.System
  ( MonadSystem (..),
    SystemT (..),
    runSystemT,
  )
where

import Aztecs.ECS
import Aztecs.ECS.Entities.Class
import Aztecs.ECS.Query
import Aztecs.ECS.System.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Data.SparseSet.Mutable
import qualified Data.SparseSet.Mutable as MS
import Data.Word

newtype SystemT s c m a = System {unSystem :: ReaderT (MSparseSet s Word32 c) m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadEntities m) => MonadEntities (SystemT s c m) where
  spawn = lift spawn
  {-# INLINE spawn #-}
  entities = Query . lift $ unQuery entities
  {-# INLINE entities #-}

instance {-# OVERLAPPING #-} (PrimMonad m, s ~ PrimState m) => MonadSystem (ComponentRef s c) (SystemT s c m) where
  query = Query . System $ do
    !s <- ask
    !as <- MS.toList s
    let go (i, c) = ComponentRef i s c
    return $ fmap (fmap go) as
  {-# INLINE query #-}

instance (MonadSystem c2 m) => MonadSystem c2 (SystemT s c m) where
  query = Query . lift $ unQuery query
  {-# INLINE query #-}

instance (PrimMonad m, PrimState m ~ s) => PrimMonad (SystemT s c m) where
  type PrimState (SystemT s c m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

runSystemT :: (PrimMonad m) => SystemT (PrimState m) c m a -> MSparseSet (PrimState m) Word32 c -> m a
runSystemT (System m) = runReaderT m
{-# INLINE runSystemT #-}
