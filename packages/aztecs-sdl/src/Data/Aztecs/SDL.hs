{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL where

import Control.Arrow (Arrow (..), returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (Asset (..), AssetServer, Handle, lookupAsset)
import qualified Data.Aztecs.Asset as Asset
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import SDL hiding (Texture, Window, windowTitle)
import qualified SDL hiding (Texture)
import qualified SDL.Image as IMG

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

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

data Camera = Camera {cameraViewport :: V2 Int}
  deriving (Show)

instance Component Camera

newtype CameraTarget = CameraTarget {cameraTargetWindow :: EntityID}
  deriving (Eq, Show)

instance Component CameraTarget

-- | Setup SDL
setup :: System () ()
setup =
  fmap (const ()) $
    Asset.setup @Texture
      &&& S.run (const initializeAll)
      &&& S.queue
        ( const $ do
            A.spawn_ . bundle $ Time 0
            A.spawn_ . bundle $ Keyboard mempty
        )

-- | Update SDL windows
update :: System () ()
update =
  const ()
    <$> ( updateTime
            &&& ( addWindows
                    >>> addCameraTargets
                    >>> addDrawTargets
                    >>> Asset.loadAssets @Texture
                    >>> keyboardInput
                )
        )

draw :: System () ()
draw = const () <$> (drawImages &&& (animateSprites >>> drawSprites)) >>> renderWindows

-- | Setup new windows.
addWindows :: System () ()
addWindows = proc () -> do
  newWindows <- S.filter (Q.entity &&& Q.fetch @_ @Window) (without @WindowRenderer) -< ()
  newWindows' <- S.run createNewWindows -< newWindows
  S.queue insertNewWindows -< newWindows'
  where
    createNewWindows newWindows = mapM createWindowRenderer newWindows
    createWindowRenderer (eId, window) = do
      sdlWindow <- createWindow (T.pack $ windowTitle window) defaultWindow
      renderer <- createRenderer sdlWindow (-1) defaultRenderer
      return (eId, sdlWindow, renderer)
    insertNewWindows newWindows' = mapM_ insertWindowRenderer newWindows'
    insertWindowRenderer (eId, window, renderer) = A.insert eId (WindowRenderer window renderer)

-- | Render windows.
renderWindows :: System () ()
renderWindows =
  let go windowDraws =
        mapM_
          ( \(window, cameraDraws) -> do
              mapM_
                ( \(camera, _, cameraTransform, cameraDraws') -> do
                    let renderer = windowRenderer window
                    rendererDrawColor renderer $= V4 0 0 0 255
                    rendererViewport renderer
                      $= Just
                        ( Rectangle
                            (P (fmap fromIntegral $ transformPosition cameraTransform))
                            (fmap fromIntegral $ cameraViewport camera)
                        )
                    clear renderer
                    mapM_ (\(d, transform) -> runDraw d transform renderer) cameraDraws'
                    present renderer
                )
                cameraDraws
          )
          windowDraws
   in proc () -> do
        cameras <-
          S.all
            ( proc () -> do
                eId <- Q.entity -< ()
                camera <- Q.fetch @_ @Camera -< ()
                cameraTarget <- Q.fetch @_ @CameraTarget -< ()
                t <- Q.fetch @_ @Transform -< ()
                returnA -< (eId, camera, cameraTarget, t)
            )
            -<
              ()
        windows <- S.all (Q.entity &&& Q.fetch @_ @WindowRenderer) -< ()
        draws <-
          S.all
            ( proc () -> do
                d <- Q.fetch @_ @Draw -< ()
                transform <- Q.fetch @_ @Transform -< ()
                target <- Q.fetch @_ @DrawTarget -< ()
                returnA -< (d, transform, target)
            )
            -<
              ()
        let cameraDraws =
              map
                ( \(eId, camera, cameraTarget, t) ->
                    ( camera,
                      cameraTarget,
                      t,
                      mapMaybe
                        ( \(d, transform, target) ->
                            if drawTargetCamera target == eId
                              then Just (d, transform)
                              else Nothing
                        )
                        draws
                    )
                )
                cameras
            windowDraws =
              map
                ( \(eId, window) ->
                    ( window,
                      filter (\(_, cameraTarget, _, _) -> cameraTargetWindow cameraTarget == eId) cameraDraws
                    )
                )
                windows
        S.run go -< windowDraws

-- | Draw target component.
-- This component can be used to specify which `Camera` to draw an entity to.
newtype DrawTarget = DrawTarget {drawTargetCamera :: EntityID}
  deriving (Eq, Show)

instance Component DrawTarget

-- | Draw component.
-- This component can be used to draw to a window.
newtype Draw = Draw {runDraw :: Transform -> Renderer -> IO ()}

instance Component Draw

-- | Add `CameraTarget` components to entities with a new `Draw` component.
addCameraTargets :: System () ()
addCameraTargets = proc () -> do
  windows <- S.all (Q.entity &&& Q.fetch @_ @Window) -< ()
  newCameras <- S.filter (Q.entity &&& Q.fetch @_ @Camera) (without @CameraTarget) -< ()
  S.queue
    ( \(newCameras, windows) -> case windows of
        (windowEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId $ CameraTarget windowEId) newCameras
        _ -> return ()
    )
    -<
      (newCameras, windows)

-- | Add `DrawTarget` components to entities with a new `Draw` component.
addDrawTargets :: System () ()
addDrawTargets = proc () -> do
  cameras <- S.all (Q.entity &&& Q.fetch @_ @Camera) -< ()
  newDraws <- S.filter (Q.entity &&& Q.fetch @_ @Draw) (without @DrawTarget) -< ()
  S.queue
    ( \(newDraws, cameras) -> case cameras of
        (cameraEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId $ DrawTarget cameraEId) newDraws
        _ -> return ()
    )
    -<
      (newDraws, cameras)

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
      (Just (Rectangle (P . fmap fromIntegral $ transformPosition transform) (fmap fromIntegral size)))
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
drawImages :: System () ()
drawImages = proc () -> do
  imgs <- S.filter (Q.entity &&& Q.fetch @_ @Image) (without @Draw) -< ()
  assets <- S.single (Q.fetch @_ @(AssetServer Texture)) -< ()
  let newAssets =
        mapMaybe (\(eId, img) -> (,img,eId) <$> lookupAsset (imageTexture img) assets) imgs
  S.queue (mapM_ go) -< newAssets
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
                        (fmap fromIntegral . P $ transformPosition transform)
                        (fmap fromIntegral $ imageSize img)
                    )
                )
                (realToFrac $ transformRotation transform)
                Nothing
                (V2 False False)
              destroyTexture sdlTexture
        )

-- | Sprite component.
data Sprite = Sprite
  { spriteTexture :: Handle Texture,
    spriteBounds :: Maybe (Rectangle Int),
    spriteSize :: V2 Int
  }
  deriving (Show)

instance Component Sprite

-- | Draw images to their target windows.
drawSprites :: System () ()
drawSprites = proc () -> do
  sprites <- S.all (Q.entity &&& Q.fetch @_ @Sprite) -< ()
  assets <- S.single (Q.fetch @_ @(AssetServer Texture)) -< ()
  let loadedAssets =
        mapMaybe (\(eId, sprite) -> (,sprite,eId) <$> lookupAsset (spriteTexture sprite) assets) sprites
  S.queue (mapM_ go) -< loadedAssets
  where
    go (texture, sprite, eId) = do
      A.insert
        eId
        ( Draw $
            \transform renderer -> do
              sdlTexture <- SDL.createTextureFromSurface renderer (textureSurface texture)
              copyEx
                renderer
                sdlTexture
                (fmap fromIntegral <$> spriteBounds sprite)
                ( Just
                    ( Rectangle
                        (fmap fromIntegral . P $ transformPosition transform)
                        (fmap fromIntegral $ spriteSize sprite)
                    )
                )
                (realToFrac $ transformRotation transform)
                Nothing
                (V2 False False)
              destroyTexture sdlTexture
        )

newtype Time = Time {elapsedMS :: Word32}
  deriving (Eq, Ord, Num, Show)

instance Component Time

updateTime :: System () ()
updateTime = proc () -> do
  t <- S.run (const SDL.ticks) -< ()
  S.mapSingle Q.set -< Time t
  returnA -< ()

data SpriteAnimation = SpriteAnimation
  { spriteAnimationSteps :: [Rectangle Int],
    spriteAnimationIndex :: Int,
    spriteAnimationMS :: Word32,
    spriteAnimationStart :: Word32
  }

instance Component SpriteAnimation

spriteAnimation :: SpriteAnimation
spriteAnimation =
  SpriteAnimation
    { spriteAnimationSteps = [],
      spriteAnimationIndex = 0,
      spriteAnimationMS = 100,
      spriteAnimationStart = 0
    }

animateSprites :: System () ()
animateSprites = proc () -> do
  currentTime <- S.single (Q.fetch @_ @Time) -< ()
  S.map_
    ( proc currentTime -> do
        sprite <- Q.fetch @_ @Sprite -< ()
        animation <- Q.fetch @_ @SpriteAnimation -< ()
        let sprite' = sprite {spriteBounds = Just $ spriteAnimationSteps animation !! spriteAnimationIndex animation}
            animation' =
              if elapsedMS currentTime - spriteAnimationStart animation > spriteAnimationMS animation
                then
                  animation
                    { spriteAnimationIndex = (spriteAnimationIndex animation + 1) `mod` length (spriteAnimationSteps animation),
                      spriteAnimationStart = elapsedMS currentTime
                    }
                else animation
        Q.set -< sprite'
        Q.set -< animation'
    )
    -<
      currentTime

-- | Keyboard state.
newtype Keyboard = Keyboard {unKeyboard :: Map Keycode InputMotion}
  deriving (Show, Semigroup, Monoid)

instance Component Keyboard

-- | Keyboard input system.
keyboardInput :: System () ()
keyboardInput = proc () -> do
  events <- S.run . const $ SDL.pollEvents -< ()
  S.mapSingle
    ( proc events -> do
        let go keyAcc event = case eventPayload event of
              KeyboardEvent keyboardEvent ->
                Keyboard $
                  Map.insert
                    (keysymKeycode $ keyboardEventKeysym keyboardEvent)
                    (keyboardEventKeyMotion keyboardEvent)
                    (unKeyboard keyAcc)
              _ -> keyAcc
        keyboard <- Q.fetch -< ()
        Q.set -< foldl' go keyboard events
    )
    -<
      events
  returnA -< ()
