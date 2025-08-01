{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.ECS.W where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query (Query (..))
import Aztecs.ECS.Queryable
import Control.Monad.Primitive
import Data.SparseSet.Strict.Mutable (MSparseSet)
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)

data W s c = W
  { wIndex :: {-# UNPACK #-} !Word32,
    wSparseSet :: {-# UNPACK #-} !(MSparseSet s Word32 c)
  }

readW :: (PrimMonad m) => W (PrimState m) c -> m c
readW r = MS.unsafeRead (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE readW #-}

writeW :: (PrimMonad m) => W (PrimState m) c -> c -> m ()
writeW r = MS.unsafeWrite (wSparseSet r) (fromIntegral $ wIndex r)
{-# INLINE writeW #-}

modifyW :: (PrimMonad m) => W (PrimState m) c -> (c -> c) -> m ()
modifyW r f = MS.unsafeModify (wSparseSet r) (fromIntegral $ wIndex r) f
{-# INLINE modifyW #-}

instance (PrimMonad m, PrimState m ~ s, Functor m) => Queryable m (W s a) where
  type QueryableAccess (W s a) = '[Write a]
  queryable (HCons s HEmpty) _ = Query $ do
    !as <- MS.toList s
    let go (i, _) = W i s
    return $ map (fmap go) as
