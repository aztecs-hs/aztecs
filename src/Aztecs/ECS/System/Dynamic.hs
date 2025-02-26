{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic
  ( DynamicSystem,
    DynamicSystemT (..),
    ArrowDynamicReaderSystem (..),
    ArrowDynamicSystem (..),
    ArrowQueueSystem (..),
    raceDyn,
    fromDynReaderSystem,
  )
where

import Aztecs.ECS.Access
import Aztecs.ECS.Query.Dynamic (DynamicQuery (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader (..))
import Aztecs.ECS.System.Dynamic.Class
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystemT (..))
import Aztecs.ECS.System.Dynamic.Reader.Class
import Aztecs.ECS.System.Queue (ArrowQueueSystem (..))
import Aztecs.ECS.View (View)
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World.Bundle
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
    runSystemDyn :: Entities -> i -> (o, View, AccessT m (), DynamicSystemT m i o)
  }
  deriving (Functor)

instance (Monad m) => Category (DynamicSystemT m) where
  id = DynamicSystem $ \_ i -> (i, mempty, pure (), id)
  DynamicSystem f . DynamicSystem g = DynamicSystem $ \w i ->
    let (b, gView, gAccess, g') = g w i
        (a, fView, fAccess, f') = f w b
     in (a, gView <> fView, gAccess >> fAccess, f' . g')

instance (Monad m) => Arrow (DynamicSystemT m) where
  arr f = DynamicSystem $ \_ i -> (f i, mempty, pure (), arr f)
  first (DynamicSystem f) = DynamicSystem $ \w (i, x) ->
    let (a, v, access, f') = f w i in ((a, x), v, access, first f')

instance (Monad m) => ArrowChoice (DynamicSystemT m) where
  left (DynamicSystem f) = DynamicSystem $ \w i -> case i of
    Left b -> let (c, v, access, f') = f w b in (Left c, v, access, left f')
    Right d -> (Right d, mempty, pure (), left (DynamicSystem f))

instance (Monad m) => ArrowLoop (DynamicSystemT m) where
  loop (DynamicSystem f) = DynamicSystem $ \w b ->
    let ((c, d), v, access, f') = f w (b, d) in (c, v, access, loop f')

instance (Monad m) => ArrowDynamicReaderSystem DynamicQueryReader (DynamicSystemT m) where
  allDyn cIds q = fromDynReaderSystem $ allDyn cIds q
  filterDyn cIds qf q = fromDynReaderSystem $ filterDyn cIds qf q

instance (Monad m) => ArrowDynamicSystem DynamicQuery (DynamicSystemT m) where
  mapDyn cIds q = DynamicSystem $ \w i ->
    let !v = V.view cIds $ archetypes w
        (o, v') = V.allDyn i q v
     in (o, v', pure (), mapDyn cIds q)
  mapSingleDyn cIds q = DynamicSystem $ \w i ->
    let s =
          mapSingleMaybeDyn cIds q
            >>> arr (fromMaybe (error "Expected a single matching entity."))
     in runSystemDyn s w i
  mapSingleMaybeDyn cIds q = DynamicSystem $ \w i ->
    let !res = V.viewSingle cIds $ archetypes w
        (res', v'') = case res of
          Just v -> let (o, v') = V.singleDyn i q v in (o, v')
          Nothing -> (Nothing, mempty)
     in (res', v'', pure (), mapSingleMaybeDyn cIds q)
  filterMapDyn cIds q f = DynamicSystem $ \w i ->
    let !v = V.filterView cIds f $ archetypes w
        (o, v') = V.allDyn i q v
     in (o, v', pure (), filterMapDyn cIds q f)

instance (Monad m) => ArrowQueueSystem Bundle (AccessT m) (DynamicSystemT m) where
  queue f = DynamicSystem $ \_ i -> ((), mempty, f i, queue f)

raceDyn :: (Monad m) => DynamicSystemT m i a -> DynamicSystemT m i b -> DynamicSystemT m i (a, b)
raceDyn (DynamicSystem f) (DynamicSystem g) = DynamicSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, v, fAccess, f') = fa
      (b, v', gAccess, g') = gbPar
   in ((a, b), v <> v', fAccess >> gAccess, raceDyn f' g')

fromDynReaderSystem :: DynamicReaderSystemT m i o -> DynamicSystemT m i o
fromDynReaderSystem (DynamicReaderSystem f) = DynamicSystem $ \w i ->
  let (o, access, f') = f w i in (o, mempty, access, fromDynReaderSystem f')
