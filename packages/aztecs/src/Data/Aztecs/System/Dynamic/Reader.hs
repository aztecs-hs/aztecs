{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Aztecs.System.Dynamic.Reader
  ( -- * Dynamic Systems
    DynamicReaderSystem (..),
    ArrowDynamicReaderSystem (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Data.Aztecs.World (World (..))

newtype DynamicReaderSystem i o = DynamicReaderSystem
  { -- | Run a dynamic system producing some output
    runReaderSystemDyn :: World -> i -> o
  }
  deriving (Functor)

instance Category DynamicReaderSystem where
  id = DynamicReaderSystem $ \_ -> \i -> i
  DynamicReaderSystem f . DynamicReaderSystem g = DynamicReaderSystem $ \w -> \i -> let b = g w i in f w b

instance Arrow DynamicReaderSystem where
  arr f = DynamicReaderSystem $ \_ -> \i -> f i
  first (DynamicReaderSystem f) = DynamicReaderSystem $ \w -> \(i, x) ->
    let a = f w i in (a, x)

instance ArrowDynamicReaderSystem DynamicReaderSystem where
  runArrowReaderSystemDyn = DynamicReaderSystem

{- TODO
raceDyn :: DynamicSystemT i a -> DynamicSystemT  i b -> DynamicSystemT  i (a, b)
raceDyn (DynamicSystemT f) (DynamicSystemT g) = DynamicSystemT $ \w -> \i -> do
  results <- parallel [fmap (\a -> (Just a, Nothing)) $ f w i, fmap (\b -> (Nothing, Just b)) $ g w i]
  ((a, v, fAccess), (b, v', gAccess)) <- case results of
    [(Just a, _), (_, Just b)] -> return (a, b)
    _ -> error "joinDyn: exception"
  return ((a, b), v <> v', fAccessT >> gAccess)
-}
