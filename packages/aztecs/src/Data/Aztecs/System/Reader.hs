{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.Aztecs.System.Reader
  ( -- * Systems
    ReaderSystem (..),
    queue,

    -- ** Queries

    -- *** Reading
    all,
    filter,
    single,

    -- *** Writing
    map,
    map_,
    filterMap,
    mapSingle,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Query (ReadsWrites (..))
import Data.Aztecs.System.Class (filterMap, map, mapSingle, map_, queue)
import Data.Aztecs.System.Dynamic.Reader (DynamicReaderSystem)
import Data.Aztecs.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (runArrowReaderSystemDyn))
import Data.Aztecs.System.Reader.Class (ArrowReaderSystem (..), all, filter, single)
import Data.Aztecs.World.Components (Components)
import Prelude hiding (all, filter, id, map, (.))
import qualified Prelude hiding (filter, id, map)

-- | System to process entities.
newtype ReaderSystem i o = SystemT
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runReaderSystem :: Components -> (DynamicReaderSystem i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance Category ReaderSystem where
  id = SystemT $ \cs -> (id, mempty, cs)
  SystemT f . SystemT g = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow ReaderSystem where
  arr f = SystemT $ \cs -> (arr f, mempty, cs)
  first (SystemT f) = SystemT $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')

{-TODO
a &&& b = SystemT $ \cs ->
  let (dynA, rwsA, cs') = runSystemT a cs
      (dynB, rwsB, cs'') = runSystemT b cs'
   in ( if Q.disjoint rwsA rwsB
          then dynA &&& dynB
          else raceDyn dynA dynB,
        rwsA <> rwsB,
        cs''
      )
-}

instance ArrowReaderSystem ReaderSystem where
  runArrowReaderSystem f = SystemT $ \cs ->
    let (g, rs, cs') = f cs in (runArrowReaderSystemDyn g, ReadsWrites rs mempty, cs')
