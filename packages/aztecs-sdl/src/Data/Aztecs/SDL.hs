{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL where

import Control.Arrow ((>>>))
import Control.Concurrent (forkIO)
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..))
import Data.Foldable (foldrM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Foreign.C (CInt)
import SDL hiding (Window, windowTitle)
import qualified SDL
import qualified SDL.Image as IMG

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
  task = S.queue (A.spawn_ assetServer) >>> S.run (const initializeAll)

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
            ( \(window, draws) -> do
                let renderer = windowRenderer window
                rendererDrawColor renderer $= V4 0 0 0 255
                clear renderer
                mapM_ (\(d, transform) -> runDraw d transform renderer) draws
                present renderer
            )
            windowDraws
     in proc () -> do
          windows <- S.all @_ @WindowRenderer -< ()
          draws <- S.all @_ @(Draw :& Transform :& WindowTarget) -< ()
          let windowDraws =
                foldr
                  ( \(eId, window) acc ->
                      let draws' =
                            foldr
                              ( \(_, d :& transform :& target) acc' ->
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
                    return $ case maybeTarget of
                      Just _ -> Nothing
                      Nothing -> Just eId
                )
                draws
            return $ catMaybes maybeTargets
        )
        -<
          draws
    S.queueWith
      ( \(newDraws, windows) -> case windows of
          (windowEId, _) : _ -> mapM_ (\eId -> A.insert eId $ WindowTarget windowEId) newDraws
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
    freeSurface surface
    destroyTexture texture

newtype AssetId = AssetId {unAssetId :: Int}
  deriving (Eq, Ord, Show)

data AssetServer = AssetServer
  { assetServerAssets :: Map AssetId Surface,
    loadingAssets :: Map AssetId (IORef (Maybe Surface)),
    nextAssetId :: AssetId
  }

instance Component AssetServer

assetServer :: AssetServer
assetServer =
  AssetServer
    { assetServerAssets = Map.empty,
      loadingAssets = Map.empty,
      nextAssetId = AssetId 0
    }

load :: FilePath -> AssetServer -> IO (AssetId, AssetServer)
load path server = do
  let assetId = nextAssetId server
  v <- newIORef Nothing
  _ <- forkIO $ do
    surface <- IMG.load path
    writeIORef v (Just surface)

  return
    ( assetId,
      server
        { loadingAssets = Map.insert assetId v (loadingAssets server),
          nextAssetId = AssetId (unAssetId assetId + 1)
        }
    )

lookupAsset :: AssetId -> AssetServer -> Maybe Surface
lookupAsset assetId server = Map.lookup assetId (assetServerAssets server)

data LoadAssets

instance System IO LoadAssets where
  task =
    S.mapM_
      ( \server ->
          foldrM
            ( \(aId, v) acc -> do
                maybeSurface <- readIORef v
                case maybeSurface of
                  Just surface ->
                    return
                      acc
                        { assetServerAssets = Map.insert aId surface (assetServerAssets acc),
                          loadingAssets = Map.delete aId (loadingAssets acc)
                        }
                  Nothing -> return acc
            )
            server
            (Map.toList $ loadingAssets server)
      )

data Image = Image
  { imageAssetId :: AssetId,
    imageSize :: V2 Int
  }
  deriving (Show)

instance Component Image

data DrawImages

instance System IO DrawImages where
  task = proc () -> do
    imgs <- S.all @_ @Image -< ()
    assets <- S.single @_ @AssetServer -< ()
    newAssets <-
      S.viewWith @_ @_ @'[Draw]
        ( \(imgs, assets) -> do
            maybeAssets <-
              mapM
                ( \(eId, img) -> case lookupAsset (imageAssetId img) assets of
                    Just surface -> do
                      return $ Just (surface, img, eId)
                    Nothing -> return Nothing
                )
                imgs
            return $ catMaybes maybeAssets
        )
        -<
          (imgs, assets)
    S.queueWith
      ( \eIds ->
          mapM_
            ( \(surface, img, eId) -> do
                A.insert
                  eId
                  ( Draw $
                      \transform renderer -> do
                        texture <- SDL.createTextureFromSurface renderer surface

                        copyEx
                          renderer
                          texture
                          Nothing
                          ( Just
                              ( Rectangle
                                  (fmap (fromIntegral @CInt . round) . P $ transformPosition transform)
                                  (fmap fromIntegral $ imageSize img)
                              )
                          )
                          (realToFrac $ transformRotation transform)
                          Nothing
                          (V2 False False)
                        destroyTexture texture
                  )
            )
            eIds
      )
      -<
        newAssets

sdlPlugin :: Scheduler IO
sdlPlugin =
  schedule @_ @PreStartup @Setup []
    <> schedule @_ @Update @AddWindows []
    <> schedule @_ @Update @AddWindowTargets []
    <> schedule @_ @Update @LoadAssets []
    <> schedule @_ @Update @DrawImages []
    <> schedule @_ @Update @RenderWindows []
