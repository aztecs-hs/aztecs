{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Aztecs.ECS.System.Dynamic.Reader
  ( DynamicReaderSystem,
    DynamicReaderSystemT (..),
    ArrowDynamicReaderSystem (..),
    raceDyn,
    runDynReaderSystem,
  )
where

import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReaderT, runDynQueryReaderT)
import Aztecs.ECS.System.Dynamic.Reader.Class
import Aztecs.ECS.Task
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Control.Parallel (par)
import qualified Data.Map as Map
import Prelude hiding (id, (.))

type DynamicReaderSystem = DynamicReaderSystemT Identity

newtype DynamicReaderSystemT m i o = DynamicReaderSystem
  { -- | Run a dynamic system producing some output
    runDynReaderSystemT :: Entities -> i -> m (o, DynamicReaderSystemT m i o)
  }
  deriving (Functor)

instance (Monad m) => Category (DynamicReaderSystemT m) where
  id = DynamicReaderSystem $ \_ i -> pure (i, id)
  DynamicReaderSystem f . DynamicReaderSystem g = DynamicReaderSystem $ \w i -> do
    (b, g') <- g w i
    (c, f') <- f w b
    return (c, f' . g')

instance (Monad m) => Arrow (DynamicReaderSystemT m) where
  arr f = DynamicReaderSystem $ \_ i -> pure (f i, arr f)
  first (DynamicReaderSystem f) = DynamicReaderSystem $ \w (i, x) -> do
    (a, f') <- f w i
    return ((a, x), first f')

instance (Monad m) => ArrowChoice (DynamicReaderSystemT m) where
  left (DynamicReaderSystem f) = DynamicReaderSystem $ \w i -> case i of
    Left b -> do
      (c, f') <- f w b
      return (Left c, left f')
    Right d -> pure (Right d, left (DynamicReaderSystem f))

instance (MonadFix m) => ArrowLoop (DynamicReaderSystemT m) where
  loop (DynamicReaderSystem f) = DynamicReaderSystem $ \w b -> do
    rec ((c, d), f') <- f w (b, d)
    return (c, loop f')

instance (Monad m) => ArrowDynamicReaderSystem (DynamicQueryReaderT m) (DynamicReaderSystemT m) where
  allDyn cIds q = DynamicReaderSystem $ \w i -> do
    let !v = V.view cIds $ archetypes w
    as <-
      if V.null v
        then runDynQueryReaderT i q (Map.keys $ entities w) A.empty
        else V.allDyn i q v
    return (as, allDyn cIds q)
  filterDyn cIds q f = DynamicReaderSystem $ \w i -> do
    let !v = V.filterView cIds f $ archetypes w
    as <- V.allDyn i q v
    return (as, filterDyn cIds q f)

instance (Monad m) => ArrowTask m (DynamicReaderSystemT m) where
  task f = DynamicReaderSystem $ \_ i -> do
    o <- f i
    return (o, task f)

runDynReaderSystem :: DynamicReaderSystem i o -> Entities -> i -> (o, DynamicReaderSystem i o)
runDynReaderSystem s es i = runIdentity $ runDynReaderSystemT s es i

raceDyn :: DynamicReaderSystem i a -> DynamicReaderSystem i b -> DynamicReaderSystem i (a, b)
raceDyn (DynamicReaderSystem f) (DynamicReaderSystem g) = DynamicReaderSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, f') = runIdentity fa
      (b, g') = runIdentity gbPar
   in pure ((a, b), raceDyn f' g')
