{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System.Dynamic
  ( -- * Dynamic Systems
    DynamicSystemT (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Concurrent.ParallelIO.Global
import Control.Monad.Identity (Identity)
import Data.Aztecs.Access (Access (..))
import Data.Aztecs.System.Dynamic.Class (ArrowDynamicSystem (..))
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicSystemReader (..))
import Data.Aztecs.View (View)
import Data.Aztecs.World (World (..))

newtype DynamicSystemT i o = DynamicSystemT
  { -- | Run a dynamic system,
    -- producing some output, an updated `View` into the `World`, and any queued `Access`.
    runSystemTDyn :: World -> (i -> (o, View, Access Identity ()))
  }
  deriving (Functor)

instance Category DynamicSystemT where
  id = DynamicSystemT $ \_ -> \i -> (i, mempty, pure ())
  DynamicSystemT f . DynamicSystemT g = DynamicSystemT $ \w -> \i ->
    let (b, gView, gAccess) = g w i
        (a, fView, fAccess) = f w b
     in (a, gView <> fView, gAccess >> fAccess)

instance Arrow DynamicSystemT where
  arr f = DynamicSystemT $ \_ -> \i -> (f i, mempty, pure ())
  first (DynamicSystemT f) = DynamicSystemT $ \w -> \(i, x) ->
    let (a, v, access) = f w i in ((a, x), v, access)

instance ArrowDynamicSystemReader DynamicSystemT where
  runArrowSystemReaderDyn f = DynamicSystemT $ \w -> \i ->
    let o = f w i in (o, mempty, pure ())

instance ArrowDynamicSystem DynamicSystemT where
  runArrowSystemDyn = DynamicSystemT

{- TODO
raceDyn :: DynamicSystemT i a -> DynamicSystemT  i b -> DynamicSystemT  i (a, b)
raceDyn (DynamicSystemT f) (DynamicSystemT g) = DynamicSystemT $ \w -> \i -> do
  results <- parallel [fmap (\a -> (Just a, Nothing)) $ f w i, fmap (\b -> (Nothing, Just b)) $ g w i]
  ((a, v, fAccess), (b, v', gAccess)) <- case results of
    [(Just a, _), (_, Just b)] -> return (a, b)
    _ -> error "joinDyn: exception"
  return ((a, b), v <> v', fAccess >> gAccess)
-}
