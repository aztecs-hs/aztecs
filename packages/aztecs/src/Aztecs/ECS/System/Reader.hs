{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Reader
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

import Aztecs.ECS.Query (ReadsWrites (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.System.Class (filterMap, map, mapSingle, map_, queue)
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystem, raceDyn)
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (runArrowReaderSystemDyn))
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..), all, filter, single)
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Prelude hiding (all, filter, id, map, (.))
import qualified Prelude hiding (filter, id, map)

-- | System to process entities.
newtype ReaderSystem i o = System
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runReaderSystem :: Components -> (DynamicReaderSystem i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance Category ReaderSystem where
  id = System $ \cs -> (id, mempty, cs)
  System f . System g = System $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow ReaderSystem where
  arr f = System $ \cs -> (arr f, mempty, cs)
  first (System f) = System $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')
  f &&& g = System $ \cs ->
    let (dynF, rwsA, cs') = runReaderSystem f cs
        (dynG, rwsB, cs'') = runReaderSystem g cs'
     in ( if Q.disjoint rwsA rwsB then dynF &&& dynG else raceDyn dynF dynG,
          rwsA <> rwsB,
          cs''
        )

instance ArrowReaderSystem ReaderSystem where
  runArrowReaderSystem f = System $ \cs ->
    let (g, rs, cs') = f cs in (runArrowReaderSystemDyn g, ReadsWrites rs mempty, cs')
