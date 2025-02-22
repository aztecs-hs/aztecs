{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System
  ( System (..),
    ArrowReaderSystem (..),
    ArrowSystem (..),
    ArrowQueueSystem (..),
    fromReader,
  )
where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.Query (Query (..), QueryFilter (..), ReadsWrites (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Reader (QueryReader (..), filterWith, filterWithout)
import Aztecs.ECS.System.Class (ArrowSystem (..))
import Aztecs.ECS.System.Dynamic (DynamicSystem (..), fromDynReaderSystem, raceDyn)
import Aztecs.ECS.System.Dynamic.Class (ArrowDynamicSystem (..))
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.System.Queue (ArrowQueueSystem (..))
import Aztecs.ECS.System.Reader (ReaderSystem (..))
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..), all, filter, single)
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Bundle (Bundle)
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import qualified Data.Foldable as F
import Prelude hiding (all, filter, id, map, (.))
import qualified Prelude hiding (filter, id, map)

-- | System to process entities.
newtype System i o = System
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runSystem :: Components -> (DynamicSystem i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance Category System where
  id = System $ \cs -> (DynamicSystem $ \_ i -> (i, mempty, pure (), id), mempty, cs)
  System f . System g = System $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow System where
  arr f = System $ \cs -> (DynamicSystem $ \_ i -> (f i, mempty, pure (), arr f), mempty, cs)
  first (System f) = System $ \cs ->
    let (f', rwsF, cs') = f cs in (first f', rwsF, cs')
  f &&& g = System $ \cs ->
    let (dynF, rwsA, cs') = runSystem f cs
        (dynG, rwsB, cs'') = runSystem g cs'
     in ( if Q.disjoint rwsA rwsB then dynF &&& dynG else raceDyn dynF dynG,
          rwsA <> rwsB,
          cs''
        )

instance ArrowReaderSystem QueryReader System where
  all q = System $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs in (allDyn rs dynQ, ReadsWrites rs mempty, cs')
  filter q qf = System $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        qf' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterDyn rs dynQ qf', ReadsWrites rs mempty, cs'')

instance ArrowSystem Query System where
  map q = System $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (mapDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')
  filterMap q qf = System $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        f' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterMapDyn (Q.reads rws <> Q.writes rws) dynQ f', rws, cs'')
  mapSingle q = System $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (mapSingleDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')
  mapSingleMaybe q = System $ \cs ->
    let !(rws, cs', dynQ) = runQuery q cs
     in (mapSingleMaybeDyn (Q.reads rws <> Q.writes rws) dynQ, rws, cs')

instance ArrowQueueSystem Bundle Access System where
  queue f = System $ \cs -> (queue f, mempty, cs)

fromReader :: ReaderSystem i o -> System i o
fromReader (ReaderSystem f) = System $ \cs ->
  let (f', rs, cs') = f cs
   in (fromDynReaderSystem f', ReadsWrites rs mempty, cs')
