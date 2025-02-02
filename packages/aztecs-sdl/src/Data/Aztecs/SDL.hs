{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.SDL where

import Control.Arrow
import Data.Aztecs
import qualified Data.Aztecs.System as S
import qualified Data.Aztecs.View as V
import SDL hiding (Window)

data Window = Window
  { windowTitle :: String
  }
  deriving (Show)

instance Component Window

newtype WindowRenderer = WindowRenderer
  { windowRenderer :: Renderer
  }
  deriving (Show)

instance Component WindowRenderer

data Setup

instance System IO Setup where
  task = S.run $ const initializeAll

data Run

instance System IO Run where
  task = proc () -> do
    windows <- S.all @_ @Window -< ()
    S.run (\ws -> print ws) -< windows
    returnA -< ()

sdlPlugin :: Scheduler IO
sdlPlugin = schedule @_ @Startup @Setup [] <> schedule @_ @Update @Run []
