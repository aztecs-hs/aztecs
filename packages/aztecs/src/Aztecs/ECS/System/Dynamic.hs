{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic (DynamicSystem (..), raceDyn) where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.Query.Dynamic (DynamicQuery)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import Aztecs.ECS.System.Dynamic.Class (ArrowDynamicSystem (..))
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.View (View, filterView, readAllDyn, view)
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Archetypes (Node)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Parallel (par)
import Data.Maybe (fromMaybe)
import Data.Set (Set)

newtype DynamicSystem i o = DynamicSystem
  { -- | Run a dynamic system,
    -- producing some output, an updated `View` into the `World`, and any queued `Access`.
    runSystemDyn :: World -> (i -> (o, View, Access ()))
  }
  deriving (Functor)

instance Category DynamicSystem where
  id = DynamicSystem $ \_ i -> (i, mempty, pure ())
  DynamicSystem f . DynamicSystem g = DynamicSystem $ \w i ->
    let (b, gView, gAccess) = g w i
        (a, fView, fAccess) = f w b
     in (a, gView <> fView, gAccess >> fAccess)

instance Arrow DynamicSystem where
  arr f = DynamicSystem $ \_ i -> (f i, mempty, pure ())
  first (DynamicSystem f) = DynamicSystem $ \w (i, x) -> let (a, v, access) = f w i in ((a, x), v, access)

instance ArrowDynamicReaderSystem DynamicQueryReader DynamicSystem where
  allDyn cIds q = DynamicSystem $ \w i ->
    let v = view cIds $ archetypes w in (readAllDyn i q v, v, pure ())
  filterDyn cIds q f = DynamicSystem $ \w i ->
    let v = filterView cIds f $ archetypes w in (readAllDyn i q v, v, pure ())

instance ArrowDynamicSystem DynamicQuery DynamicSystem where
  mapDyn cIds q = DynamicSystem $ mapDyn' cIds q
  mapSingleDyn cIds q = DynamicSystem $ mapSingleDyn' cIds q
  mapSingleMaybeDyn cIds q = DynamicSystem $ mapSingleMaybeDyn' cIds q
  filterMapDyn cIds q f = DynamicSystem $ filterMapDyn' cIds q f
  queueDyn f = DynamicSystem $ queueDyn' f

raceDyn :: DynamicSystem i a -> DynamicSystem i b -> DynamicSystem i (a, b)
raceDyn (DynamicSystem f) (DynamicSystem g) = DynamicSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, v, fAccess) = fa
      (b, v', gAccess) = gbPar
   in ((a, b), v <> v', fAccess >> gAccess)

type DynamicSystemT i o = World -> i -> (o, View, Access ())

-- | Map all matching entities, storing the updated entities.
mapDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystemT i [o]
mapDyn' cIds q w =
  let !v = V.view cIds $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

mapSingleDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystemT i o
mapSingleDyn' cIds q w i =
  let !(maybeO, v, access) = mapSingleMaybeDyn' cIds q w i
      !o = fromMaybe (error "Expected a single matching entity.") maybeO
   in (o, v, access)

-- | Map all matching entities, storing the updated entities.
mapSingleMaybeDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystemT i (Maybe o)
mapSingleMaybeDyn' cIds q w i =
  let !res = V.viewSingle cIds $ archetypes w
   in case res of
        Just v -> let (o, v') = V.singleDyn i q v in (o, v', pure ())
        Nothing -> (Nothing, mempty, pure ())

filterMapDyn' ::
  Set ComponentID ->
  DynamicQuery i o ->
  (Node -> Bool) ->
  DynamicSystemT i [o]
filterMapDyn' cIds q f w =
  let !v = V.filterView cIds f $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

queueDyn' :: (i -> Access ()) -> DynamicSystemT i ()
queueDyn' f _ i = ((), mempty, f i)
