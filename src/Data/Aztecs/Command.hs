{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Command where

import Control.Monad.State (MonadIO, MonadState (..), StateT (runStateT))
import Data.Aztecs (Entity)
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World as W
import Data.Data (Typeable)

newtype Command m a = Command {unCommand :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

runCommand :: Command m a -> World -> m (a, World)
runCommand (Command cmd) w = runStateT cmd w

spawn :: (Typeable c, Monad m) => c -> Command m Entity
spawn c = Command $ do
  w <- get
  let (e, w') = W.spawn c w
  put w'
  return e

insert :: (Monad m, Typeable c) => Entity -> c -> Command m ()
insert e c = Command $ do
  w <- get
  let w' = W.insert e c w
  put w'
  return ()
