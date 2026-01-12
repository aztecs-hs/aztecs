{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Aztecs.ECS.Query
  ( Query (..),
    withQuery,
    runQuery,
    mapQueryM_,
    foldQuery,
  )
where

import Aztecs.ECS.W (Runner (..))
import Data.Vector.Fusion.Stream.Monadic (Step (..), Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import GHC.Exts (SPEC (..))

data Query m s a = Query
  { querySize :: {-# UNPACK #-} !Int,
    queryStream :: Stream m (Maybe a)
  }

instance (Monad m) => Functor (Query m s) where
  fmap f (Query sz s) = Query sz (SM.map (fmap f) s)
  {-# INLINE fmap #-}

instance (Monad m) => Applicative (Query m s) where
  pure x = Query 1 (SM.singleton (Just x))
  {-# INLINE pure #-}

  Query sz1 s1 <*> Query sz2 s2 =
    let !sz = min sz1 sz2
     in Query sz $ SM.zipWith (<*>) s1 s2
  {-# INLINE (<*>) #-}
withQuery :: (Monad m) => (forall s. Query m s a -> m b) -> Int -> Stream m (Maybe a) -> m b
withQuery f sz stream = f (Query sz stream)
{-# INLINE withQuery #-}

runQuery :: (Monad m) => Query m s a -> m [a]
runQuery (Query _ s) = SM.toList $ SM.catMaybes s
{-# INLINE runQuery #-}

mapQueryM_ :: (Monad m) => (a -> Runner s m ()) -> Query m s a -> Runner s m ()
mapQueryM_ f (Query _ (Stream step s0)) = go SPEC s0
  where
    go !_ !s = Runner $ do
      r <- step s
      case r of
        Yield (Just !a) !s' -> unsafeRunRunner (f a) >> unsafeRunRunner (go SPEC s')
        Yield Nothing !s' -> unsafeRunRunner (go SPEC s')
        Skip !s' -> unsafeRunRunner (go SPEC s')
        Done -> return ()
{-# INLINE mapQueryM_ #-}

foldQuery :: (Monad m) => (b -> a -> b) -> b -> Query m s a -> m b
foldQuery f z (Query _ s) = SM.foldl' (\acc ma -> maybe acc (f acc) ma) z s
{-# INLINE foldQuery #-}
