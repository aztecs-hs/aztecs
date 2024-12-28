{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Command
  ( Command (..),
    spawn,
    insert,
    Edit (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.State (StateT (..))
import qualified Control.Monad.State as S
import Control.Monad.Writer (WriterT)
import Data.Aztecs.World (Component, Entity, World)
import qualified Data.Aztecs.World as W
import Data.Dynamic (Typeable)
import Data.Proxy
import Prelude hiding (all)

data Edit where
  Spawn :: (Component c) => Entity -> (Proxy c) -> Edit
  Insert :: (Component c) => Entity -> (Proxy c) -> Edit

-- | Command to update the `World`.
newtype Command m a = Command (StateT World (WriterT [Edit] m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Spawn a `Component` and return its `Entity`.
spawn :: (Component a, Typeable a) => a -> Command IO Entity
spawn a = Command $ do
  w <- S.get
  (e, w') <- liftIO $ W.spawn a w
  S.put $ w'
  return e

-- | Insert a `Component` into an `Entity`.
insert :: (Component a, Typeable a) => Entity -> a -> Command IO ()
insert e a =  Command $ do
  w <- S.get
  w' <- liftIO $ W.insert e a w
  S.put w'
