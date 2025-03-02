{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Aztecs.ECS.System.Dynamic
  ( DynamicSystem,
    DynamicSystemT (..),
    ArrowDynamicReaderSystem (..),
    ArrowDynamicSystem (..),
    raceDyn,
    fromDynReaderSystem,
  )
where

import Aztecs.ECS.Query.Dynamic (DynamicQueryT)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReaderT)
import Aztecs.ECS.System.Dynamic.Class
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystemT (..))
import Aztecs.ECS.System.Dynamic.Reader.Class
import Aztecs.ECS.Task
import Aztecs.ECS.View (View)
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Control.Parallel (par)
import Data.Maybe (fromMaybe)
import Prelude hiding (id, (.))

type DynamicSystem = DynamicSystemT Identity

newtype DynamicSystemT m i o = DynamicSystem
  { -- | Run a dynamic system,
    -- producing some output, an updated `View` into the `World`, and any queued `Access`.
    runSystemDyn :: Entities -> i -> m (o, View, DynamicSystemT m i o)
  }
  deriving (Functor)

instance (Monad m) => Category (DynamicSystemT m) where
  id = DynamicSystem $ \_ i -> pure (i, mempty, id)
  DynamicSystem f . DynamicSystem g = DynamicSystem $ \w i -> do
    (b, gView, g') <- g w i
    (a, fView, f') <- f w b
    return (a, gView <> fView, f' . g')

instance (Monad m) => Arrow (DynamicSystemT m) where
  arr f = DynamicSystem $ \_ i -> pure (f i, mempty, arr f)
  first (DynamicSystem f) = DynamicSystem $ \w (i, x) -> do
    (a, v, f') <- f w i
    return ((a, x), v, first f')

instance (Monad m) => ArrowChoice (DynamicSystemT m) where
  left (DynamicSystem f) = DynamicSystem $ \w i -> case i of
    Left b -> do
      (c, v, f') <- f w b
      return (Left c, v, left f')
    Right d -> pure (Right d, mempty, left (DynamicSystem f))

instance (MonadFix m) => ArrowLoop (DynamicSystemT m) where
  loop (DynamicSystem f) = DynamicSystem $ \w b -> do
    rec ((c, d), v, f') <- f w (b, d)
    return (c, v, loop f')

instance (Monad m) => ArrowDynamicReaderSystem (DynamicQueryReaderT m) (DynamicSystemT m) where
  allDyn cIds q = fromDynReaderSystem $ allDyn cIds q
  filterDyn cIds qf q = fromDynReaderSystem $ filterDyn cIds qf q

instance (Monad m) => ArrowDynamicSystem (DynamicQueryT m) (DynamicSystemT m) where
  mapDyn cIds q = DynamicSystem $ \w i -> do
    let !v = V.view cIds $ archetypes w
    (o, v') <- V.mapDyn i q v
    return (o, v', mapDyn cIds q)
  mapSingleDyn cIds q = DynamicSystem $ \w i ->
    let s =
          mapSingleMaybeDyn cIds q
            >>> arr (fromMaybe (error "Expected a single matching entity."))
     in runSystemDyn s w i
  mapSingleMaybeDyn cIds q = DynamicSystem $ \w i -> do
    let !res = V.viewSingle cIds $ archetypes w
    (res', v'') <- case res of
      Just v -> V.mapSingleDyn i q v
      Nothing -> pure (Nothing, mempty)
    return (res', v'', mapSingleMaybeDyn cIds q)
  filterMapDyn cIds q f = DynamicSystem $ \w i -> do
    let !v = V.filterView cIds f $ archetypes w
    (o, v') <- V.mapDyn i q v
    return (o, v', filterMapDyn cIds q f)

instance (Monad m) => ArrowTask m (DynamicSystemT m) where
  task f = DynamicSystem $ \_ i -> do
    o <- f i
    return (o, mempty, task f)

raceDyn :: DynamicSystem i a -> DynamicSystem i b -> DynamicSystem i (a, b)
raceDyn (DynamicSystem f) (DynamicSystem g) = DynamicSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, v, f') = runIdentity fa
      (b, v', g') = runIdentity gbPar
   in return ((a, b), v <> v', raceDyn f' g')

fromDynReaderSystem :: (Monad m) => DynamicReaderSystemT m i o -> DynamicSystemT m i o
fromDynReaderSystem (DynamicReaderSystem f) = DynamicSystem $ \w i -> do
  (o, f') <- f w i
  return (o, mempty, fromDynReaderSystem f')
