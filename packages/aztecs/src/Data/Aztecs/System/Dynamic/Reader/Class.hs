{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.System.Dynamic.Reader.Class
  ( -- * Reader
    ArrowDynamicReaderSystem (..),

    -- ** Queries

    -- *** Reading
    allDyn,
    allDyn',
    filterDyn,
    filterDyn',
    singleDyn,
  )
where

import Control.Arrow (Arrow (..), (>>>))
import Data.Aztecs.Component (ComponentID)
import Data.Aztecs.Query.Dynamic.Reader (DynamicQueryReader)
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetypes (Node)
import Data.Set (Set)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

class (Arrow a) => ArrowDynamicReaderSystem a where
  runArrowReaderSystemDyn :: (World -> i -> o) -> a i o

allDyn ::
  (ArrowDynamicReaderSystem arr) =>
  Set ComponentID ->
  DynamicQueryReader i o ->
  arr i [o]
allDyn cIds q = runArrowReaderSystemDyn $ allDyn' cIds q

allDyn' :: Set ComponentID -> DynamicQueryReader i o -> World -> i -> [o]
allDyn' cIds q = \w -> let !v = V.view cIds $ archetypes w in \i -> V.readAllDyn i q v

filterDyn ::
  (ArrowDynamicReaderSystem arr) =>
  Set ComponentID ->
  DynamicQueryReader i o ->
  (Node -> Bool) ->
  arr i [o]
filterDyn cIds q f = runArrowReaderSystemDyn $ filterDyn' cIds q f

filterDyn' ::
  Set ComponentID ->
  DynamicQueryReader i o ->
  (Node -> Bool) ->
  World ->
  i ->
  [o]
filterDyn' cIds q f = \w ->
  let !v = V.filterView cIds f $ archetypes w in \i -> V.readAllDyn i q v

singleDyn ::
  (ArrowDynamicReaderSystem arr) =>
  Set ComponentID ->
  DynamicQueryReader () a ->
  arr () a
singleDyn cIds q =
  (allDyn cIds q)
    >>> arr
      ( \as -> case as of
          [a] -> a
          _ -> error "TODO"
      )
