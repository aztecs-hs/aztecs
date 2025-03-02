{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Aztecs.ECS.System.Reader
  ( ReaderSystem,
    ReaderSystemT (..),
    ArrowReaderSystem (..),
  )
where

import Aztecs.ECS.Query.Reader
import Aztecs.ECS.System.Dynamic.Reader
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..))
import Aztecs.ECS.Task
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
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
  id = ReaderSystem (id,mempty,)
  ReaderSystem f . ReaderSystem g = ReaderSystem $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance (Monad m) => Arrow (ReaderSystemT m) where
  arr f = ReaderSystem (arr f,mempty,)
  first (ReaderSystem f) = ReaderSystem $ \cs ->
    let (f', rwsF, cs') = f cs in (first f', rwsF, cs')

instance (Monad m) => ArrowChoice (ReaderSystemT m) where
  left (ReaderSystem f) = ReaderSystem $ \cs -> let (f', rwsF, cs') = f cs in (left f', rwsF, cs')

instance (MonadFix m) => ArrowLoop (ReaderSystemT m) where
  loop (ReaderSystem f) = ReaderSystem $ \cs -> let (f', rwsF, cs') = f cs in (loop f', rwsF, cs')

instance (Monad m) => ArrowReaderSystem (QueryReaderT m) (ReaderSystemT m) where
  all q = ReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs in (allDyn rs dynQ, rs, cs')
  single q = ReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs in (singleDyn rs dynQ, rs, cs')
  singleMaybe q = ReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs in (singleMaybeDyn rs dynQ, rs, cs')
  filter q qf = ReaderSystem $ \cs ->
    let !(rs, cs', dynQ) = runQueryReader q cs
        !(dynQf, cs'') = runQueryFilter qf cs'
        qf' n =
          F.all (\cId -> A.member cId $ nodeArchetype n) (filterWith dynQf)
            && F.all (\cId -> not (A.member cId $ nodeArchetype n)) (filterWithout dynQf)
     in (filterDyn rs dynQ qf', rs, cs'')

instance (Monad m) => ArrowTask m (ReaderSystemT m) where
  task f = ReaderSystem (task f,mempty,)
