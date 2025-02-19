{-# LANGUAGE BangPatterns #-}

module Data.Aztecs.System.Dynamic.Class
  ( ArrowDynamicSystem (..),
    DynamicSystemT,
    mapDyn',
    filterMapDyn',
    queueDyn',
  )
where

import Data.Aztecs.Access (Access)
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query.Dynamic (DynamicQuery)
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetypes (Node (..))
import Data.Set (Set)

type DynamicSystemT i o = World -> i -> (o, View, Access ())

class (ArrowDynamicReaderSystem arr) => ArrowDynamicSystem arr where
  runArrowSystemDyn :: DynamicSystemT i o -> arr i o

  -- | Map all matching entities, storing the updated entities.
  mapDyn :: Set ComponentID -> DynamicQuery i o -> arr i [o]
  mapDyn cIds q = runArrowSystemDyn $ mapDyn' cIds q

  filterMapDyn ::
    Set ComponentID ->
    DynamicQuery i o ->
    (Node -> Bool) ->
    arr i [o]
  filterMapDyn cIds q f = runArrowSystemDyn $ filterMapDyn' cIds q f

  queueDyn :: (i -> Access ()) -> arr i ()
  queueDyn f = runArrowSystemDyn $ \_ i -> ((), mempty, f i)

-- | Map all matching entities, storing the updated entities.
mapDyn' :: Set ComponentID -> DynamicQuery i o -> DynamicSystemT i [o]
mapDyn' cIds q w =
  let !v = V.view cIds $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

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
