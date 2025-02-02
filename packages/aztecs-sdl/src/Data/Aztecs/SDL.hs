{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL where

import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Foreign.C (CInt)
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
    let draw windowDraws =
          mapM_
            ( \(window, draws) ->
                mapM_
                  ( \(d, transform) -> do
                      let renderer = windowRenderer window
                      rendererDrawColor renderer $= V4 0 0 0 255
                      clear renderer
                      runDraw d transform renderer
                      present renderer
                  )
                  draws
            )
            windowDraws
     in proc () -> do
          windows <- S.all @_ @WindowRenderer -< ()
          draws <- S.all @_ @(Draw :& (Transform :& WindowTarget)) -< ()
          let windowDraws =
                foldr
                  ( \(eId, window) acc ->
                      let draws' =
                            foldr
                              ( \(_, d :& (transform :& target)) acc' ->
                                  if unWindowTarget target == eId
                                    then (d, transform) : acc'
                                    else acc'
                              )
                              []
                              draws
                       in (window, draws') : acc
                  )
                  []
                  windows
          S.run draw -< windowDraws

newtype WindowTarget = WindowTarget {unWindowTarget :: EntityID}
  deriving (Eq, Show)

instance Component WindowTarget

newtype Draw = Draw {runDraw :: Transform -> Renderer -> IO ()}

instance Component Draw

data AddWindowTargets

instance System IO AddWindowTargets where
  task = proc () -> do
    windows <- S.all @_ @WindowRenderer -< ()
    draws <- S.all @_ @Draw -< ()
    newDraws <-
      S.viewWith @_ @_ @'[WindowTarget]
        ( \draws -> do
            maybeTargets <-
              mapM
                ( \(eId, _) -> do
                    maybeTarget <- S.lookupView @_ @WindowTarget eId
                    case maybeTarget of
                      Just _ -> return Nothing
                      Nothing -> return $ Just eId
                )
                draws
            return $ catMaybes maybeTargets
        )
        -<
          draws
    S.queueWith
      ( \(newDraws, windows) -> case windows of
          (windowEId, _) : _ -> do
            mapM_
              ( \eId -> do
                  _ <- A.insert eId $ WindowTarget windowEId
                  return ()
              )
              newDraws
          _ -> return ()
      )
      -<
        (newDraws, windows)

rect :: V2 Int -> Draw
rect size = Draw $
  \transform renderer -> do
    surface <- createRGBSurface (fmap fromIntegral size) RGBA8888
    surfaceRenderer <- createSoftwareRenderer surface
    rendererDrawColor surfaceRenderer $= V4 255 0 0 255
    fillRect surfaceRenderer Nothing
    texture <- SDL.createTextureFromSurface renderer surface
    copyEx
      renderer
      texture
      Nothing
      (Just (Rectangle (fmap (fromIntegral @CInt . round) . P $ transformPosition transform) (fmap fromIntegral size)))
      (realToFrac $ transformRotation transform)
      Nothing
      (V2 False False)

sdlPlugin :: Scheduler IO
sdlPlugin =
  schedule @_ @Startup @Setup []
    <> schedule @_ @Update @AddWindows []
    <> schedule @_ @Update @AddWindowTargets []
    <> schedule @_ @Update @RenderWindows []
