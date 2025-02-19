{-# LANGUAGE BangPatterns #-}

module Aztecs.ECS.System.Dynamic.Reader.Class
  ( ArrowDynamicReaderSystem (..),
    allDyn',
    filterDyn',
  )
where

import Control.Arrow (Arrow (..), (>>>))
import Aztecs.ECS.Component (ComponentID)
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader)
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Archetypes (Node)
import Data.Set (Set)

class (Arrow arr) => ArrowDynamicReaderSystem arr where
  runArrowReaderSystemDyn :: (World -> i -> o) -> arr i o

  allDyn :: Set ComponentID -> DynamicQueryReader i o -> arr i [o]
  allDyn cIds q = runArrowReaderSystemDyn $ allDyn' cIds q

  filterDyn :: Set ComponentID -> DynamicQueryReader i o -> (Node -> Bool) -> arr i [o]
  filterDyn cIds q f = runArrowReaderSystemDyn $ filterDyn' cIds q f

  singleDyn :: Set ComponentID -> DynamicQueryReader () a -> arr () a
  singleDyn cIds q =
    allDyn cIds q
      >>> arr
        ( \as -> case as of
            [a] -> a
            _ -> error "TODO"
        )

allDyn' :: Set ComponentID -> DynamicQueryReader i o -> World -> i -> [o]
allDyn' cIds q w = let !v = V.view cIds $ archetypes w in \i -> V.readAllDyn i q v

filterDyn' ::
  Set ComponentID ->
  DynamicQueryReader i o ->
  (Node -> Bool) ->
  World ->
  i ->
  [o]
filterDyn' cIds q f w = let !v = V.filterView cIds f $ archetypes w in \i -> V.readAllDyn i q v
