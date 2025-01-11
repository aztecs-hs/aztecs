{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Command where

import Control.Monad (void)
import Control.Monad.State (MonadIO, MonadState (..), StateT (runStateT))
import Data.Aztecs (EntityID)
import Data.Aztecs.World (Insertable, World)
import qualified Data.Aztecs.World as W

newtype Command m a = Command {unCommand :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

runCommand :: Command m a -> World -> m (a, World)
runCommand (Command cmd) w = runStateT cmd w

spawn :: (Monad m, Insertable a) => a -> Command m EntityID
spawn c = Command $ do
  w <- get
  let (e, w') = W.spawn c w
  put w'
  return e

spawn_ :: (Monad m, Insertable a) => a -> Command m ()
spawn_ = void . spawn

insert :: (Monad m, Insertable a) => EntityID -> a -> Command m ()
insert e c = Command $ do
  w <- get
  let w' = W.insert e c w
  put w'
  return ()
