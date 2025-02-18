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

import Data.Aztecs.Access (Access (..))
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query.Dynamic (DynamicQuery)
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicSystemReader (..))
import Data.Aztecs.View (View)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetypes (Node (..))
import Data.Set (Set)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

class (ArrowDynamicSystemReader m a) => ArrowDynamicSystem m a where
  runArrowSystemDyn :: (World -> (i -> m (o, View, Access m ()))) -> a i o

-- | Map all matching entities, storing the updated entities.
mapDyn :: (ArrowDynamicSystem m arr) => Set ComponentID -> DynamicQuery m i o -> arr i [o]
mapDyn cIds q = runArrowSystemDyn $ mapDyn' cIds q

-- | Map all matching entities, storing the updated entities.
mapDyn' ::
  (Monad m) =>
  Set ComponentID ->
  DynamicQuery m i o ->
  World ->
  (i -> m ([o], View, Access m ()))
mapDyn' cIds q = \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> fmap (\(a, v') -> (a, v', pure ())) (V.allDyn i q v)

filterMapDyn ::
  (ArrowDynamicSystem m arr) =>
  Set ComponentID ->
  DynamicQuery m i o ->
  (Node -> Bool) ->
  arr i [o]
filterMapDyn cIds q f = runArrowSystemDyn $ filterMapDyn' cIds q f

filterMapDyn' ::
  (Monad m) =>
  Set ComponentID ->
  DynamicQuery m i o ->
  (Node -> Bool) ->
  World ->
  (i -> m ([o], View, Access m ()))
filterMapDyn' cIds q f = \w ->
  let !v = V.filterView cIds f $ archetypes w
   in \i -> fmap (\(a, v') -> (a, v', pure ())) (V.allDyn i q v)

queueDyn :: (ArrowDynamicSystem m arr) => (i -> Access m ()) -> arr i ()
queueDyn f = runArrowSystemDyn $ \_ -> \i -> return ((), mempty, f i)

queueDyn' :: (Monad m) => (i -> Access m ()) -> World -> (i -> m ((), View, Access m ()))
queueDyn' f _ = \i -> return ((), mempty, f i)