{-# LANGUAGE DeriveFunctor #-}

module Aztecs.ECS.Query where

import Data.Maybe

newtype Query m a = Query {unQuery :: m [Maybe a]}
  deriving (Functor)

instance (Monad m) => Applicative (Query m) where
  pure x = Query $ return [Just x]
  {-# INLINE pure #-}
  Query f <*> Query x = Query $ do
    fs <- f
    zipWith (<*>) fs <$> x
  {-# INLINE (<*>) #-}

runQuery :: (Monad m) => Query m a -> m [a]
runQuery (Query q) = catMaybes <$> q
{-# INLINE runQuery #-}
