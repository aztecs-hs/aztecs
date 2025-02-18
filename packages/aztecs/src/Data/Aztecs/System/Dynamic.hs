{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System.Dynamic
  ( -- * Dynamic Systems
    DynamicSystemT (..),
    raceDyn,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Concurrent.ParallelIO.Global
import Data.Aztecs.Access (Access (..))
import Data.Aztecs.System.Dynamic.Class (ArrowDynamicSystem (..))
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicSystemReader (..))
import Data.Aztecs.View (View)
import Data.Aztecs.World (World (..))

newtype DynamicSystemT m i o = DynamicSystemT
  { -- | Run a dynamic system,
    -- producing some output, an updated `View` into the `World`, and any queued `Access`.
    runSystemTDyn :: World -> (i -> m (o, View, Access m ()))
  }
  deriving (Functor)

instance (Monad m) => Category (DynamicSystemT m) where
  id = DynamicSystemT $ \_ -> \i -> return (i, mempty, pure ())
  DynamicSystemT f . DynamicSystemT g = DynamicSystemT $ \w -> \i -> do
    (b, gView, gAccess) <- g w i
    (a, fView, fAccess) <- f w b
    return (a, gView <> fView, gAccess >> fAccess)

instance (Monad m) => Arrow (DynamicSystemT m) where
  arr f = DynamicSystemT $ \_ -> \i -> return (f i, mempty, pure ())
  first (DynamicSystemT f) = DynamicSystemT $ \w -> \(i, x) -> do
    (a, v, access) <- f w i
    return ((a, x), v, access)

instance (Monad m) => ArrowDynamicSystemReader m (DynamicSystemT m) where
  runArrowSystemReaderDyn f = DynamicSystemT $ \w -> \i -> do
    o <- f w i
    return (o, mempty, pure ())

instance (Monad m) => ArrowDynamicSystem m (DynamicSystemT m) where
  runArrowSystemDyn = DynamicSystemT

raceDyn :: DynamicSystemT IO i a -> DynamicSystemT IO i b -> DynamicSystemT IO i (a, b)
raceDyn (DynamicSystemT f) (DynamicSystemT g) = DynamicSystemT $ \w -> \i -> do
  results <- parallel [fmap (\a -> (Just a, Nothing)) $ f w i, fmap (\b -> (Nothing, Just b)) $ g w i]
  ((a, v, fAccess), (b, v', gAccess)) <- case results of
    [(Just a, _), (_, Just b)] -> return (a, b)
    _ -> error "joinDyn: exception"
  return ((a, b), v <> v', fAccess >> gAccess)
