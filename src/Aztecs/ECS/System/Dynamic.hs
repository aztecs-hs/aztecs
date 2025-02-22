{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic
  ( DynamicSystem (..),
    ArrowDynamicReaderSystem (..),
    ArrowDynamicSystem (..),
    ArrowQueueSystem (..),
    raceDyn,
    fromDynReaderSystem,
  )
where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Query.Dynamic (DynamicQuery)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import Aztecs.ECS.System.Dynamic.Class (ArrowDynamicSystem (..))
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystem (..))
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.System.Queue (ArrowQueueSystem (..))
import Aztecs.ECS.View (View)
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Bundle (Bundle)
import Control.Arrow (Arrow (..), (>>>))
import Control.Category (Category (..))
import Control.Parallel (par)
import Data.Maybe (fromMaybe)
import Prelude hiding (id, (.))

newtype DynamicSystem i o = DynamicSystem
  { -- | Run a dynamic system,
    -- producing some output, an updated `View` into the `World`, and any queued `Access`.
    runSystemDyn :: World -> i -> (o, View, Access (), DynamicSystem i o)
  }
  deriving (Functor)

instance Category DynamicSystem where
  id = DynamicSystem $ \_ i -> (i, mempty, pure (), id)
  DynamicSystem f . DynamicSystem g = DynamicSystem $ \w i ->
    let (b, gView, gAccess, g') = g w i
        (a, fView, fAccess, f') = f w b
     in (a, gView <> fView, gAccess >> fAccess, f' . g')

instance Arrow DynamicSystem where
  arr f = DynamicSystem $ \_ i -> (f i, mempty, pure (), arr f)
  first (DynamicSystem f) = DynamicSystem $ \w (i, x) ->
    let (a, v, access, f') = f w i in ((a, x), v, access, first f')

instance ArrowDynamicReaderSystem DynamicQueryReader DynamicSystem where
  allDyn cIds q = fromDynReaderSystem $ allDyn cIds q
  filterDyn cIds qf q = fromDynReaderSystem $ filterDyn cIds qf q

instance ArrowDynamicSystem DynamicQuery DynamicSystem where
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

instance ArrowQueueSystem Bundle Access DynamicSystem where
  queue f = DynamicSystem $ \_ i -> ((), mempty, f i, queue f)

raceDyn :: DynamicSystem i a -> DynamicSystem i b -> DynamicSystem i (a, b)
raceDyn (DynamicSystem f) (DynamicSystem g) = DynamicSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, v, fAccess, f') = fa
      (b, v', gAccess, g') = gbPar
   in ((a, b), v <> v', fAccess >> gAccess, raceDyn f' g')

fromDynReaderSystem :: DynamicReaderSystem i o -> DynamicSystem i o
fromDynReaderSystem (DynamicReaderSystem f) = DynamicSystem $ \w i ->
  let (o, access, f') = f w i in (o, mempty, access, fromDynReaderSystem f')
