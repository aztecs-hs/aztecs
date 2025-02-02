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
    newWindows <-
      S.viewWith @_ @_ @'[WindowRenderer]
        ( \windows -> do
            maybeNewWindows <-
              mapM
                ( \(eId, window) -> do
                    res <- S.lookupView eId
                    case res of
                      Just (WindowRenderer _ _) -> return Nothing
                      Nothing -> return . Just $ (eId, window)
                )
                windows
            return $ catMaybes maybeNewWindows
        )
        -<
          windows
    newWindows' <-
      S.run
        ( \newWindows -> do
            mapM
              ( \(eId, window) -> do
                  sdlWindow <- createWindow (T.pack $ windowTitle window) defaultWindow
                  renderer <- createRenderer sdlWindow (-1) defaultRenderer
                  return (eId, sdlWindow, renderer)
              )
              newWindows
        )
        -<
          newWindows
    S.queueWith
      ( \newWindows' -> do
          mapM_
            ( \(eId, window, renderer) -> do
                A.insert eId (WindowRenderer window renderer)
            )
            newWindows'
      )
      -<
        newWindows'

data RenderWindows

instance System IO RenderWindows where
  task =
    let go windows = do
          mapM_
            ( \(_, WindowRenderer _ renderer) -> do
                rendererDrawColor renderer $= V4 0 0 0 255
                clear renderer
                present renderer
            )
            windows
     in proc () -> do
          windows <- S.all @_ @WindowRenderer -< ()
          S.run go -< windows

sdlPlugin :: Scheduler IO
sdlPlugin =
  schedule @_ @Startup @Setup []
    <> schedule @_ @Update @AddWindows []
    <> schedule @_ @Update @RenderWindows []
