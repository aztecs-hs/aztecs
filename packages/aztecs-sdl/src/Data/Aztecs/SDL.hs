{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL where

import Control.Arrow (returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (Asset (..), AssetServer, Handle, lookupAsset)
import qualified Data.Aztecs.Asset as Asset
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Foreign.C (CInt)
import SDL hiding (Texture, Window, windowTitle)
import qualified SDL hiding (Texture)
import qualified SDL.Image as IMG

-- | Window component.
data Window = Window
  { windowTitle :: String
  }
  deriving (Show)

instance Component Window

-- | Window renderer component.
data WindowRenderer = WindowRenderer
  { windowRendererRaw :: SDL.Window,
    windowRenderer :: Renderer
  }
  deriving (Show)

instance Component WindowRenderer

-- | Setup SDL
setup :: System IO () ()
setup = Asset.setup @Texture >>> S.run (const initializeAll)

-- | Update SDL windows
update :: System IO () ()
update =
  addWindows
    >>> addWindowTargets
    >>> renderWindows
    >>> drawImages
    >>> Asset.loadAssets @Texture

-- | Setup new windows.
addWindows :: System IO () ()
addWindows = proc () -> do
  newWindows <- S.allFilter (Q.fetchWithId @_ @Window) (without @WindowRenderer) -< ()
  newWindows' <- S.run createNewWindows -< newWindows
  S.queueWith insertNewWindows -< newWindows'
  where
    createNewWindows newWindows = mapM createWindowRenderer newWindows
    createWindowRenderer (eId, window) = do
      sdlWindow <- createWindow (T.pack $ windowTitle window) defaultWindow
      renderer <- createRenderer sdlWindow (-1) defaultRenderer
      return (eId, sdlWindow, renderer)
    insertNewWindows newWindows' = mapM_ insertWindowRenderer newWindows'
    insertWindowRenderer (eId, window, renderer) = A.insert eId (WindowRenderer window renderer)

-- | Render windows.
renderWindows :: System IO () ()
renderWindows =
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
        windows <- S.all (Q.fetchWithId @_ @WindowRenderer) -< ()
        draws <-
          S.all
            ( proc () -> do
                d <- Q.fetch @_ @Draw -< ()
                transform <- Q.fetch @_ @Transform -< ()
                target <- Q.fetch @_ @WindowTarget -< ()
                returnA -< (d, transform, target)
            )
            -<
              ()
        let windowDraws =
              foldr
                ( \(eId, window) acc ->
                    let draws' =
                          foldr
                            ( \(d, transform, target) acc' ->
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

-- | Window target component.
-- This component can be used to specify which `Window` to draw an entity to.
newtype WindowTarget = WindowTarget {unWindowTarget :: EntityID}
  deriving (Eq, Show)

instance Component WindowTarget

-- | Draw component.
-- This component can be used to draw to a window.
newtype Draw = Draw {runDraw :: Transform -> Renderer -> IO ()}

instance Component Draw

-- | Add `WindowTarget` components to entities with a new `Draw` component.
addWindowTargets :: System IO () ()
addWindowTargets = proc () -> do
  windows <- S.all (Q.fetchWithId @_ @WindowRenderer) -< ()
  newDraws <- S.allFilter (Q.fetchWithId @_ @Draw )(without @WindowTarget) -< ()
  S.queueWith
    ( \(newDraws, windows) -> case windows of
        (windowEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId $ WindowTarget windowEId) newDraws
        _ -> return ()
    )
    -<
      (newDraws, windows)

-- | Draw a rectangle.
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

-- | Texture asset.
newtype Texture = Texture {textureSurface :: Surface}

instance Asset Texture where
  loadAsset path = Texture <$> IMG.load path

-- | Image component.
data Image = Image
  { imageTexture :: Handle Texture,
    imageSize :: V2 Int
  }
  deriving (Show)

instance Component Image

-- | Draw images to their target windows.
drawImages :: System IO () ()
drawImages = proc () -> do
  imgs <- S.allFilter (Q.fetchWithId @_ @Image) (without @Draw) -< ()
  assets <- S.single (Q.fetch @_ @(AssetServer Texture)) -< ()
  let newAssets =
        mapMaybe (\(eId, img) -> (,img,eId) <$> lookupAsset (imageTexture img) assets) imgs
  S.queueWith (mapM_ go) -< newAssets
  where
    go (texture, img, eId) = do
      A.insert
        eId
        ( Draw $
            \transform renderer -> do
              sdlTexture <- SDL.createTextureFromSurface renderer (textureSurface texture)
              copyEx
                renderer
                sdlTexture
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
              destroyTexture sdlTexture
        )
