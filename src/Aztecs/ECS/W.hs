{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Aztecs.ECS.W (W (..), Runner (..), runRunner, liftRunner) where

import Control.Monad.IO.Class (MonadIO (..))
import Unsafe.Coerce (unsafeCoerce)

-- | A scoped monad wrapper that ensures operations cannot escape their scope.
-- The phantom type @s@ tracks the scope, similar to the ST monad.
newtype Runner s m a = Runner { unsafeRunRunner :: m a }
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

-- | Lift a monadic action into the Runner monad.
liftRunner :: m a -> Runner s m a
liftRunner = Runner
{-# INLINE liftRunner #-}

-- | Run a Runner action. This should only be called within a scoped context.
-- The forall ensures the scope cannot escape.
runRunner :: (Monad m) => (forall s. Runner s m a) -> m a
runRunner r = unsafeRunRunner r
{-# INLINE runRunner #-}

-- | Read-write 'Queryable' component access.
-- The methods return 'Runner s m' to ensure operations stay within scope.
data W s m c = W
  { readW :: !(Runner s m c),
    writeW :: !(c -> Runner s m ()),
    modifyW :: !((c -> c) -> Runner s m ())
  }

type role W phantom representational nominal
