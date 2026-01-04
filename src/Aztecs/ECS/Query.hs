{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Aztecs.ECS.Query
  ( Query (..),
    runQuery,
    mapQueryM_,
    foldQuery,
    transQuery,
  )
where

import Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import GHC.Exts (SPEC (..))

data Query m a = forall s. Query
  { querySize :: {-# UNPACK #-} !Int,
    queryStream :: Stream m (Maybe a)
  }

instance (Monad m) => Functor (Query m) where
  fmap f (Query sz s) = Query sz (SM.map (fmap f) s)
  {-# INLINE fmap #-}

instance (Monad m) => Applicative (Query m) where
  pure x = Query 1 (SM.singleton (Just x))
  {-# INLINE pure #-}

  Query sz1 s1 <*> Query sz2 s2 =
    let !sz = min sz1 sz2
     in Query sz $ SM.zipWith (<*>) s1 s2
  {-# INLINE (<*>) #-}

-- | Run a query and collect results into a list.
runQuery :: (Monad m) => Query m a -> m [a]
runQuery (Query _ s) = SM.toList $ SM.catMaybes s
{-# INLINE runQuery #-}

mapQueryM_ :: (Monad m) => (a -> m ()) -> Query m a -> m ()
mapQueryM_ f (Query _ (Stream step s0)) = go SPEC s0
  where
    go !_ !s = do
      r <- step s
      case r of
        Yield (Just !a) !s' -> f a >> go SPEC s'
        Yield Nothing !s' -> go SPEC s'
        Skip !s' -> go SPEC s'
        Done -> return ()
{-# INLINE mapQueryM_ #-}

-- | Fold over query results.
foldQuery :: (Monad m) => (b -> a -> b) -> b -> Query m a -> m b
foldQuery f z (Query _ s) = SM.foldl' (\acc ma -> maybe acc (f acc) ma) z s
{-# INLINE foldQuery #-}

-- | Transform a query from one monad to another.
transQuery :: (Monad m, Monad n) => (forall x. m x -> n x) -> Query m a -> Query n a
transQuery nat (Query sz s) = Query sz (SM.trans nat s)
{-# INLINE transQuery #-}
