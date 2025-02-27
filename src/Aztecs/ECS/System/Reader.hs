{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Reader
  ( ReaderSystem,
    ReaderSystemT (..),
    ArrowReaderSystem (..),
    ArrowQueueSystem (..),
  )
where

import Aztecs.ECS.Access (AccessT)
import Aztecs.ECS.Query.Reader
import Aztecs.ECS.System.Dynamic.Reader
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..))
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Bundle (Bundle)
import Aztecs.ECS.World.Components (ComponentID, Components)
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import qualified Data.Foldable as F
import Data.Set (Set)
import Prelude hiding (id, (.))

type ReaderSystem = ReaderSystemT Identity

-- | System to process entities.
newtype ReaderSystemT m i o = ReaderSystem
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runReaderSystem :: Components -> (DynamicReaderSystemT m i o, Set ComponentID, Components)
  }
  deriving (Functor)

instance (Monad m) => Category (ReaderSystemT m) where
  id = ReaderSystem $ \cs -> (id, mempty, cs)
  ReaderSystem f . ReaderSystem g = ReaderSystem $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance (Monad m) => Arrow (ReaderSystemT m) where
  arr f = ReaderSystem $ \cs -> (arr f, mempty, cs)
  first (ReaderSystem f) = ReaderSystem $ \cs ->
    let (f', rwsF, cs') = f cs in (first f', rwsF, cs')
  f &&& g = ReaderSystem $ \cs ->
    let (dynF, rwsA, cs') = runReaderSystem f cs
        (dynG, rwsB, cs'') = runReaderSystem g cs'
     in (raceDyn dynF dynG, rwsA <> rwsB, cs'')

instance (Monad m) => ArrowChoice (ReaderSystemT m) where
  left (ReaderSystem f) = ReaderSystem $ \cs -> let (f', rwsF, cs') = f cs in (left f', rwsF, cs')

instance (Monad m) => ArrowLoop (ReaderSystemT m) where
  loop (ReaderSystem f) = ReaderSystem $ \cs -> let (f', rwsF, cs') = f cs in (loop f', rwsF, cs')

instance (Monad m) => ArrowReaderSystem QueryReader (ReaderSystemT m) where
  all q = ReaderSystem $ \cs ->
    let !(qs, cs') = runQueryReader q cs
     in (allDyn (queryReaderStateReads qs) $ queryReaderStateDyn qs, queryReaderStateReads qs, cs')
  filter q qf = ReaderSystem $ \cs ->
    let !(qs, cs') = runQueryReader q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        qf' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterDyn (queryReaderStateReads qs) (queryReaderStateDyn qs) qf', queryReaderStateReads qs, cs'')

instance (Monad m) => ArrowQueueSystem Bundle (AccessT m) (ReaderSystemT m) where
  queue f = ReaderSystem $ \cs -> (queue f, mempty, cs)
