{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aztecs.Edit where

import Control.Monad (void)
import Control.Monad.State (MonadIO, MonadState (..), StateT (runStateT))
import Data.Aztecs (EntityID)
import Data.Aztecs.World (Insertable, World)
import qualified Data.Aztecs.World as W

newtype Edit m a = Edit {unEdit :: StateT World m a}
  deriving (Functor, Applicative, Monad, MonadIO)

runEdit :: Edit m a -> World -> m (a, World)
runEdit (Edit cmd) w = runStateT cmd w

spawn :: (Monad m, Insertable a) => a -> Edit m EntityID
spawn c = Edit $ do
  w <- get
  let (e, w') = W.spawn c w
  put w'
  return e

spawn_ :: (Monad m, Insertable a) => a -> Edit m ()
spawn_ = void . spawn

insert :: (Monad m, Insertable a) => EntityID -> a -> Edit m ()
insert e c = Edit $ do
  w <- get
  let w' = W.insert e c w
  put w'
  return ()
