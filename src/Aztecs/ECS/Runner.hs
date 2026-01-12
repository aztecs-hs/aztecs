{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Aztecs.ECS.Runner (Runner (..), runRunner, liftRunner) where

import Control.Monad.IO.Class (MonadIO (..))

-- | System runner
newtype Runner s m a = Runner {unsafeRunRunner :: m a}
  deriving (Functor)

type role Runner phantom representational nominal

instance (Applicative m) => Applicative (Runner s m) where
  pure = Runner . pure
  {-# INLINE pure #-}
  Runner f <*> Runner a = Runner (f <*> a)
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (Runner s m) where
  Runner m >>= f = Runner (m >>= unsafeRunRunner . f)
  {-# INLINE (>>=) #-}

instance (MonadIO m) => MonadIO (Runner s m) where
  liftIO = Runner . liftIO
  {-# INLINE liftIO #-}

-- | Lift a monadic action `m` into a Runner.
liftRunner :: m a -> Runner s m a
liftRunner = Runner
{-# INLINE liftRunner #-}

runRunner :: (Monad m) => (forall s. Runner s m a) -> m a
runRunner r = unsafeRunRunner r
{-# INLINE runRunner #-}
