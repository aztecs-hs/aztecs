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
import Data.Bits
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.SparseSet.Mutable (MSparseSet)
import qualified Data.SparseSet.Mutable as MS
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

data EntityCounter = EntityCounter
  { entitiesNextGeneration :: Word32,
    entitiesGenerations :: IntMap Word32,
    entitiesNextIndex :: Word32,
    entitiesFreeIndicies :: [Word32]
  }

emptyEntityCounter :: EntityCounter
emptyEntityCounter = EntityCounter 0 IntMap.empty 0 []

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
