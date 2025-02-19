{-# LANGUAGE BangPatterns #-}

module Aztecs.ECS.System.Dynamic.Class
  ( ArrowDynamicSystem (..),
    DynamicSystem,
    mapDyn',
    mapSingleDyn',
    mapSingleMaybeDyn',
    filterMapDyn',
    queueDyn',
  )
where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.Query.Dynamic (DynamicQuery)
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.View (View)
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Archetypes (Node (..))
import Data.Maybe (fromMaybe)
import Data.Set (Set)

type DynamicSystem i o = World -> i -> (o, View, Access ())

class (ArrowDynamicReaderSystem arr) => ArrowDynamicSystem arr where
  runArrowSystemDyn :: DynamicSystem i o -> arr i o

  -- | Map all matching entities, storing the updated entities.
  mapDyn :: Set ComponentID -> DynamicQuery i o -> arr i [o]
  mapDyn cIds q = runArrowSystemDyn $ mapDyn' cIds q

  mapSingleDyn :: Set ComponentID -> DynamicQuery i o -> arr i o
  mapSingleDyn cIds q = runArrowSystemDyn $ mapSingleDyn' cIds q

  mapSingleMaybeDyn :: Set ComponentID -> DynamicQuery i o -> arr i (Maybe o)
  mapSingleMaybeDyn cIds q = runArrowSystemDyn $ mapSingleMaybeDyn' cIds q

  filterMapDyn ::
    Set ComponentID ->
    DynamicQuery i o ->
    (Node -> Bool) ->
    arr i [o]
  filterMapDyn cIds q f = runArrowSystemDyn $ filterMapDyn' cIds q f

  queueDyn :: (i -> Access ()) -> arr i ()
  queueDyn f = runArrowSystemDyn $ \_ i -> ((), mempty, f i)

-- | Map all matching entities, storing the updated entities.
mapDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystem i [o]
mapDyn' cIds q w =
  let !v = V.view cIds $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

mapSingleDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystem i o
mapSingleDyn' cIds q w i =
  let !(maybeO, v, access) = mapSingleMaybeDyn' cIds q w i
      !o = fromMaybe (error "Expected a single matching entity.") maybeO
   in (o, v, access)

-- | Map all matching entities, storing the updated entities.
mapSingleMaybeDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystem i (Maybe o)
mapSingleMaybeDyn' cIds q w i =
  let !res = V.viewSingle cIds $ archetypes w
   in case res of
        Just v -> let (o, v') = V.singleDyn i q v in (o, v', pure ())
        Nothing -> (Nothing, mempty, pure ())

filterMapDyn' ::
  Set ComponentID ->
  DynamicQuery i o ->
  (Node -> Bool) ->
  DynamicSystem i [o]
filterMapDyn' cIds q f w =
  let !v = V.filterView cIds f $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

queueDyn' :: (i -> Access ()) -> DynamicSystem i ()
queueDyn' f _ i = ((), mempty, f i)
