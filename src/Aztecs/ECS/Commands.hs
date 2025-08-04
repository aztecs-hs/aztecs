{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Aztecs.ECS.Commands where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans
import Aztecs.ECS.Queryable

newtype Commands t m a = Commands {unCommands :: m (a, t m ())}
  deriving (Functor)

instance (Monad (t m), Monad m) => Applicative (Commands t m) where
  pure x = Commands $ pure (x, pure ())
  {-# INLINE pure #-}
  Commands mf <*> Commands mx = Commands $ do
    (f, w1) <- mf
    (x, w2) <- mx
    return (f x, w1 >> w2)
  {-# INLINE (<*>) #-}

instance (Monad (t m), Monad m) => Monad (Commands t m) where
  Commands mx >>= f = Commands $ do
    (x, w1) <- mx
    (y, w2) <- unCommands (f x)
    return (y, w1 >> w2)
  {-# INLINE (>>=) #-}

instance (MonadTrans t) => MonadTrans (Commands t) where
  lift m = Commands $ do
    x <- m
    return (x, lift $ pure ())
  {-# INLINE lift #-}

instance (MonadTrans t, Monad (t m), MonadIO m) => MonadIO (Commands t m) where
  liftIO io = Commands $ do
    x <- liftIO io
    return (x, lift $ pure ())
  {-# INLINE liftIO #-}

instance (MonadTrans t, Monad (t m), PrimMonad m) => PrimMonad (Commands t m) where
  type PrimState (Commands t m) = PrimState m
  primitive f = Commands $ do
    x <- primitive f
    return (x, lift $ pure ())
  {-# INLINE primitive #-}

queue :: (Applicative m) => t m () -> Commands t m ()
queue action = Commands $ pure ((), action)
{-# INLINE queue #-}
