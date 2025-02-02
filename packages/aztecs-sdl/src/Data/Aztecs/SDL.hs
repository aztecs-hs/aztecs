{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.SDL where

import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.System as S
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import SDL hiding (Window, windowTitle)
import qualified SDL

data Window = Window
  { windowTitle :: String
  }
  deriving (Show)

instance Component Window

data WindowRenderer = WindowRenderer
  { windowRendererRaw :: SDL.Window,
    windowRenderer :: Renderer
  }
  deriving (Show)

instance Component WindowRenderer

data Setup

instance System IO Setup where
  task = S.run $ const initializeAll

data AddWindows

instance System IO AddWindows where
  task = proc () -> do
    windows <- S.all @_ @Window -< ()
    newWindows <- S.viewWith @_ @_ @'[WindowRenderer] filterNewWindows -< windows
    newWindows' <- S.run createNewWindows -< newWindows
    S.queueWith insertNewWindows -< newWindows'
    where
      filterNewWindows windows = do
        maybeNewWindows <- mapM checkWindow windows
        return $ catMaybes maybeNewWindows
      checkWindow (eId, window) = do
        res <- S.lookupView eId
        return $ case res of
          Just (WindowRenderer _ _) -> Nothing
          Nothing -> Just (eId, window)
      createNewWindows newWindows = mapM createWindowRenderer newWindows
      createWindowRenderer (eId, window) = do
        sdlWindow <- createWindow (T.pack $ windowTitle window) defaultWindow
        renderer <- createRenderer sdlWindow (-1) defaultRenderer
        return (eId, sdlWindow, renderer)
      insertNewWindows newWindows' = mapM_ insertWindowRenderer newWindows'
      insertWindowRenderer (eId, window, renderer) = A.insert eId (WindowRenderer window renderer)

data RenderWindows

instance System IO RenderWindows where
  task =
    let go' (_, WindowRenderer _ renderer) = do
          rendererDrawColor renderer $= V4 0 0 0 255
          clear renderer
          present renderer
        go windows = mapM_ go' windows
     in proc () -> do
          windows <- S.all @_ @WindowRenderer -< ()
          S.run go -< windows

sdlPlugin :: Scheduler IO
sdlPlugin =
  schedule @_ @Startup @Setup []
    <> schedule @_ @Update @AddWindows []
    <> schedule @_ @Update @RenderWindows []
