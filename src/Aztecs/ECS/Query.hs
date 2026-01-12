{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Aztecs.ECS.Query
  ( Query (..),
    mapQueryM_,
    foldQueryM,
    lookupQuery,
  )
where

import Aztecs.ECS.W
import Aztecs.Entity
import Data.Word
import GHC.Exts (SPEC (..))

-- | Query into the World.
data Query m s a = Query
  { -- | Size of the query result
    querySize :: !Int,
    -- | Fetch function for single-entity lookup and iteration
    fetchQuery :: !(Word32 -> m (Maybe a))
  }

instance (Monad m) => Functor (Query m s) where
  fmap f (Query sz fetch) = Query sz (\i -> fmap (fmap f) (fetch i))
  {-# INLINE fmap #-}

instance (Monad m) => Applicative (Query m s) where
  pure x = Query 1 (\_ -> return (Just x))
  {-# INLINE pure #-}

  Query sz1 f1 <*> Query sz2 f2 =
    let !sz = min sz1 sz2
        fetch i = do
          mf <- f1 i
          ma <- f2 i
          return (mf <*> ma)
     in Query sz fetch
  {-# INLINE (<*>) #-}

-- | Lookup a single entity's query result.
lookupQuery :: (Monad m) => Entity -> Query m s a -> m (Maybe a)
lookupQuery entity (Query _ fetch) = fetch (entityIndex entity)
{-# INLINE lookupQuery #-}

-- | Map a function over all entities in the query.
mapQueryM_ :: (Monad m) => (a -> Runner s m ()) -> Query m s a -> Runner s m ()
mapQueryM_ f (Query sz fetch) = go SPEC 0
  where
    go !_ !i
      | i >= fromIntegral sz = Runner $ return ()
      | otherwise = Runner $ do
          ma <- fetch i
          case ma of
            Just !a -> unsafeRunRunner (f a) >> unsafeRunRunner (go SPEC (i + 1))
            Nothing -> unsafeRunRunner (go SPEC (i + 1))
{-# INLINE mapQueryM_ #-}

-- | Fold over all entities in the query.
foldQueryM :: (Monad m) => (b -> a -> m b) -> b -> Query m s a -> m b
foldQueryM f z (Query sz fetch) = go 0 z
  where
    go !i !acc
      | i >= fromIntegral sz = return acc
      | otherwise = do
          ma <- fetch i
          case ma of
            Just !a -> f acc a >>= go (i + 1)
            Nothing -> go (i + 1) acc
{-# INLINE foldQueryM #-}
