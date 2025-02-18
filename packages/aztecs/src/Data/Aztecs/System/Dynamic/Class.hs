{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.System.Dynamic.Class
  ( ArrowDynamicSystem (..),
    mapDyn,
    mapDyn',
    filterMapDyn,
    filterMapDyn',
    queueDyn,
    queueDyn',
  )
where

import Control.Monad.Identity (Identity)
import Data.Aztecs.Access (Access (..))
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query.Dynamic (DynamicQuery)
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetypes (Node (..))
import Data.Set (Set)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

class (ArrowDynamicReaderSystem a) => ArrowDynamicSystem a where
  runArrowSystemDyn :: (World -> (i -> (o, View, Access Identity ()))) -> a i o

-- | Map all matching entities, storing the updated entities.
mapDyn :: (ArrowDynamicSystem arr) => Set ComponentID -> DynamicQuery i o -> arr i [o]
mapDyn cIds q = runArrowSystemDyn $ mapDyn' cIds q

-- | Map all matching entities, storing the updated entities.
mapDyn' ::
  Set ComponentID ->
  DynamicQuery i o ->
  World ->
  (i -> ([o], View, Access Identity ()))
mapDyn' cIds q = \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

filterMapDyn ::
  (ArrowDynamicSystem arr) =>
  Set ComponentID ->
  DynamicQuery i o ->
  (Node -> Bool) ->
  arr i [o]
filterMapDyn cIds q f = runArrowSystemDyn $ filterMapDyn' cIds q f

filterMapDyn' ::
  Set ComponentID ->
  DynamicQuery i o ->
  (Node -> Bool) ->
  World ->
  (i -> ([o], View, Access Identity ()))
filterMapDyn' cIds q f = \w ->
  let !v = V.filterView cIds f $ archetypes w
   in \i -> let (o, v') = V.allDyn i q v in (o, v', pure ())

queueDyn :: (ArrowDynamicSystem arr) => (i -> Access Identity ()) -> arr i ()
queueDyn f = runArrowSystemDyn $ \_ -> \i -> ((), mempty, f i)

queueDyn' :: (i -> Access Identity ()) -> World -> (i -> ((), View, Access Identity ()))
queueDyn' f _ = \i -> ((), mempty, f i)
