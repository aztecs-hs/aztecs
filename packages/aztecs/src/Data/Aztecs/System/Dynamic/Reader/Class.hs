{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Aztecs.System.Dynamic.Reader.Class
  ( -- * Reader
    ArrowDynamicSystemReader (..),

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

class (Monad m, Arrow a) => ArrowDynamicSystemReader m a where
  runArrowSystemReaderDyn :: (World -> (i -> m o)) -> a i o

allDyn ::
  (Monad m, ArrowDynamicSystemReader m arr) =>
  Set ComponentID ->
  DynamicQueryReader m i o ->
  arr i [o]
allDyn cIds q = runArrowSystemReaderDyn $ allDyn' cIds q

allDyn' ::
  (Monad m) =>
  Set ComponentID ->
  DynamicQueryReader m i o ->
  World ->
  i ->
  m [o]
allDyn' cIds q = \w ->
  let !v = V.view cIds $ archetypes w
   in \i -> V.readAllDyn i q v

filterDyn ::
  (Monad m, ArrowDynamicSystemReader m arr) =>
  Set ComponentID ->
  DynamicQueryReader m i o ->
  (Node -> Bool) ->
  arr i [o]
filterDyn cIds q f = runArrowSystemReaderDyn $ filterDyn' cIds q f

filterDyn' ::
  (Monad m) =>
  Set ComponentID ->
  DynamicQueryReader m i o ->
  (Node -> Bool) ->
  World ->
  i ->
  m [o]
filterDyn' cIds q f = \w ->
  let !v = V.filterView cIds f $ archetypes w
   in \i -> V.readAllDyn i q v

singleDyn ::
  (Monad m, ArrowDynamicSystemReader m arr) =>
  Set ComponentID ->
  DynamicQueryReader m () a ->
  arr () a
singleDyn cIds q =
  (allDyn cIds q)
    >>> arr
      ( \as -> case as of
          [a] -> a
          _ -> error "TODO"
      )
