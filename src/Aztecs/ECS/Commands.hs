{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Commands where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans

newtype Commands t m a = Commands {unCommands :: m (a, t m ())}
  deriving (Functor)

instance (MonadTrans t, Monad m) => Applicative (Commands t m) where
  pure x = Commands $ pure (x, lift $ pure ())
  Commands mf <*> Commands mx = Commands $ do
    (f, w1) <- mf
    (x, w2) <- mx
    return (f x, w1 >> w2)

instance (MonadTrans t, Monad m) => Monad (Commands t m) where
  Commands mx >>= f = Commands $ do
    (x, w1) <- mx
    (y, w2) <- unCommands (f x)
    return (y, w1 >> w2)

instance (MonadTrans t) => MonadTrans (Commands t) where
  lift m = Commands $ do
    x <- m
    return (x, lift $ pure ())

instance (MonadTrans t, MonadIO m) => MonadIO (Commands t m) where
  liftIO io = Commands $ do
    x <- liftIO io
    return (x, lift $ pure ())

instance (MonadTrans t, PrimMonad m) => PrimMonad (Commands t m) where
  type PrimState (Commands t m) = PrimState m
  primitive f = Commands $ do
    x <- primitive f
    return (x, lift $ pure ())

queue :: (Applicative m) => t m () -> Commands t m ()
queue action = Commands $ pure ((), action)
