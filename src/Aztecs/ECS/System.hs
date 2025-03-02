{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Aztecs.ECS.System
  ( System,
    SystemT (..),
    ArrowReaderSystem (..),
    ArrowSystem (..),
    fromReader,
  )
where

import Aztecs.ECS.Query (QueryFilter (..), QueryT (..), ReadsWrites (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Reader (DynamicQueryFilter (..), QueryReaderT)
import Aztecs.ECS.System.Class
import Aztecs.ECS.System.Dynamic
import Aztecs.ECS.System.Reader
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Archetypes (Node (..))
import Aztecs.ECS.World.Components (Components)
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import qualified Data.Foldable as F
import Prelude hiding (all, filter, id, map, (.))
import qualified Prelude hiding (filter, id, map)

type System = SystemT Identity

-- | System to process entities.
newtype SystemT m i o = System
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runSystem :: Components -> (DynamicSystemT m i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance (Monad m) => Category (SystemT m) where
  id = System (id,mempty,)
  System f . System g = System $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance (Monad m) => Arrow (SystemT m) where
  arr f = System (arr f,mempty,)
  first (System f) = System $ \cs ->
    let (f', rwsF, cs') = f cs in (first f', rwsF, cs')

instance (Monad m) => ArrowChoice (SystemT m) where
  left (System f) = System $ \cs -> let (f', rwsF, cs') = f cs in (left f', rwsF, cs')

instance (MonadFix m) => ArrowLoop (SystemT m) where
  loop (System f) = System $ \cs -> let (f', rwsF, cs') = f cs in (loop f', rwsF, cs')

instance (Monad m) => ArrowReaderSystem (QueryReaderT m) (SystemT m) where
  all = fromReader . all
  filter q = fromReader . filter q

instance (Monad m) => ArrowSystem (QueryT m) (SystemT m) where
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

fromReader :: (Monad m) => ReaderSystemT m i o -> SystemT m i o
fromReader (ReaderSystem f) = System $ \cs ->
  let (f', rs, cs') = f cs in (fromDynReaderSystem f', ReadsWrites rs mempty, cs')
