{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Reader
  ( -- * Systems
    ReaderSystem (..),

    -- ** Queries
  )
where

import Aztecs.ECS.Query.Reader
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystem, raceDyn)
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.System.Queue (ArrowQueueSystem (..))
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..))
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (ComponentID, Components)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (id, (.))

-- | System to process entities.
newtype ReaderSystem i o = ReaderSystem
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runReaderSystem :: Components -> (DynamicReaderSystem i o, Set ComponentID, Components)
  }
  deriving (Functor)

instance Category ReaderSystem where
  id = ReaderSystem $ \cs -> (id, mempty, cs)
  ReaderSystem f . ReaderSystem g = ReaderSystem $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow ReaderSystem where
  arr f = ReaderSystem $ \cs -> (arr f, mempty, cs)
  first (ReaderSystem f) = ReaderSystem $ \cs ->
    let (f', rwsF, cs') = f cs
     in (first f', rwsF, cs')
  f &&& g = ReaderSystem $ \cs ->
    let (dynF, rwsA, cs') = runReaderSystem f cs
        (dynG, rwsB, cs'') = runReaderSystem g cs'
     in (raceDyn dynF dynG, rwsA <> rwsB, cs'')

instance ArrowReaderSystem QueryReader ReaderSystem where
  all q = ReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs in (allDyn rs dynQ, rs, cs')
  filter q qf = ReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        qf' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterDyn rs dynQ qf', rs, cs'')

instance ArrowQueueSystem ReaderSystem where
  queue f = ReaderSystem $ \cs -> (queue f, mempty, cs)
