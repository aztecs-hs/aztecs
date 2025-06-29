{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS where

import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.SparseSet.Strict (SparseSet)
import qualified Data.SparseSet.Strict as S
import Data.SparseSet.Strict.Mutable (MSparseSet)
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (lookup)

newtype Entity = Entity {unEntity :: Word64}
  deriving (Eq, Ord, Show)

mkEntity :: Word32 -> Word32 -> Entity
mkEntity index generation = Entity $ (fromIntegral generation `shiftL` 32) .|. fromIntegral index

entityIndex :: Entity -> Word32
entityIndex (Entity e) = fromIntegral (e .&. 0xFFFFFFFF)

entityGeneration :: Entity -> Word32
entityGeneration (Entity e) = fromIntegral ((e `shiftR` 32) .&. 0xFFFFFFFF)

class (Monad m) => MonadEntities m where
  spawn :: m Entity
  entities :: Query m Entity

data EntityCounter = EntityCounter
  { entitiesNextGeneration :: Word32,
    entitiesGenerations :: IntMap Word32,
    entitiesNextIndex :: Word32,
    entitiesFreeIndicies :: [Word32]
  }

emptyEntityCounter :: EntityCounter
emptyEntityCounter = EntityCounter 0 IntMap.empty 0 []

newtype EntitiesT m a = EntitiesT {unEntitiesT :: StateT EntityCounter m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Monad m) => MonadEntities (EntitiesT m) where
  spawn = EntitiesT $ do
    EntityCounter gen gens index free <- get
    let (i, nextIndex, free') = case free of
          (i' : rest) -> (i', index, rest)
          [] -> (index, index + 1, [])
        nextGeneration = gen + 1
        gens' = IntMap.insert (fromIntegral nextGeneration) i gens
    put $ EntityCounter nextGeneration gens' nextIndex free'
    return $ mkEntity i gen
  entities = Query $
    EntitiesT $ do
      EntityCounter _ gens _ _ <- get
      let go (i, g) = Just $ mkEntity (fromIntegral i) g
      return . map go $ IntMap.toList gens

instance (PrimMonad m) => PrimMonad (EntitiesT m) where
  type PrimState (EntitiesT m) = PrimState m
  primitive = EntitiesT . lift . primitive
  {-# INLINE primitive #-}

runEntitiesT :: (Monad m) => EntitiesT m a -> EntityCounter -> m (a, EntityCounter)
runEntitiesT (EntitiesT m) = runStateT m
{-# INLINE runEntitiesT #-}

class (Monad m) => MonadQuery c m | m -> c where
  query :: Query m c

class (Monad m) => MonadAccess c m | m -> c where
  insert :: Entity -> c -> m ()
  lookup :: Entity -> m (Maybe c)

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

instance {-# OVERLAPPING #-} (Monad m) => MonadQuery c (AccessT c m) where
  query = Query . AccessT $ S.toList <$> get
  {-# INLINE query #-}

instance (MonadQuery c2 m) => MonadQuery c2 (AccessT c m) where
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

newtype Query m a = Query {unQuery :: m [Maybe a]}
  deriving (Functor)

instance (Monad m) => Applicative (Query m) where
  pure x = Query $ return [Just x]
  {-# INLINE pure #-}
  Query f <*> Query x = Query $ do
    fs <- f
    zipWith (<*>) fs <$> x
  {-# INLINE (<*>) #-}

runQuery :: (Monad m) => Query m a -> m [a]
runQuery (Query q) = catMaybes <$> q
{-# INLINE runQuery #-}

data ComponentRef s c = ComponentRef
  { componentRefIndex :: {-# UNPACK #-} !Word32,
    componentRefSparseSet :: {-# UNPACK #-} !(MSparseSet s Word32 c),
    unComponentRef :: !c
  }

readComponentRef :: (PrimMonad m) => ComponentRef (PrimState m) c -> m c
readComponentRef r = do
  res <- MS.unsafeRead (componentRefSparseSet r) (fromIntegral $ componentRefIndex r)
  case res of
    Just c -> return c
    Nothing -> error "readComponentRef: impossible"
{-# INLINE readComponentRef #-}

writeComponentRef :: (PrimMonad m) => ComponentRef (PrimState m) c -> c -> m ()
writeComponentRef r = MS.unsafeWrite (componentRefSparseSet r) (fromIntegral $ componentRefIndex r)
{-# INLINE writeComponentRef #-}

modifyComponentRef :: (PrimMonad m) => ComponentRef (PrimState m) c -> (c -> c) -> m ()
modifyComponentRef r f = MS.unsafeModify (componentRefSparseSet r) (fromIntegral $ componentRefIndex r) f
{-# INLINE modifyComponentRef #-}

newtype SystemT s c m a = System {unSystem :: ReaderT (MSparseSet s Word32 c) m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadEntities m) => MonadEntities (SystemT s c m) where
  spawn = lift spawn
  {-# INLINE spawn #-}
  entities = Query . lift $ unQuery entities
  {-# INLINE entities #-}

instance {-# OVERLAPPING #-} (PrimMonad m, s ~ PrimState m) => MonadQuery (ComponentRef s c) (SystemT s c m) where
  query = Query . System $ do
    !s <- ask
    !as <- MS.toList s
    let go (i, c) = ComponentRef i s c
    return $ fmap (fmap go) as
  {-# INLINE query #-}

instance (MonadQuery c2 m) => MonadQuery c2 (SystemT s c m) where
  query = Query . lift $ unQuery query
  {-# INLINE query #-}

instance (PrimMonad m, PrimState m ~ s) => PrimMonad (SystemT s c m) where
  type PrimState (SystemT s c m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

runSystemT :: (PrimMonad m) => SystemT (PrimState m) c m a -> MSparseSet (PrimState m) Word32 c -> m a
runSystemT (System m) = runReaderT m
{-# INLINE runSystemT #-}
