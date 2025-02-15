{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL
  ( -- * Window components
    Window (..),
    WindowRenderer (..),

    -- * Camera components
    Camera (..),
    CameraTarget (..),

    -- * Draw components
    Draw (..),
    rect,
    DrawTarget (..),

    -- * Input

    -- ** Keyboard input
    KeyboardInput (..),
    isKeyPressed,
    wasKeyPressed,
    wasKeyReleased,

    -- ** Mouse input
    MouseInput (..),

    -- * Time
    Time (..),

    -- * Systems
    setup,
    update,
    draw,

    -- ** Primitive systems
    addWindows,
    renderWindows,
    addCameraTargets,
    addDrawTargets,
    handleInput,
    updateTime,
    clearKeyboard,
    clearKeyboardQuery,
    clearMouseInput,
    clearMouseInputQuery,
  )
where

import Control.Arrow (Arrow (..), returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word32)
import SDL hiding (Texture, Window, windowTitle)
import qualified SDL hiding (Texture)

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
    S.run (const initializeAll)
      &&& S.queue
        ( const $ do
            A.spawn_ . bundle $ Time 0
            A.spawn_ . bundle $ KeyboardInput mempty mempty
            A.spawn_ . bundle $ MouseInput (P $ V2 0 0) (V2 0 0) mempty
        )

-- | Update SDL windows
update :: System () ()
update =
  const ()
    <$> ( updateTime
            &&& ( addWindows
                    >>> addCameraTargets
                    >>> addDrawTargets
                    >>> handleInput
                )
        )

draw :: System () ()
draw = const () <$> (renderWindows &&& clearKeyboard &&& clearMouseInput)

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

newtype Time = Time {elapsedMS :: Word32}
  deriving (Eq, Ord, Num, Show)

instance Component Time

updateTime :: System () ()
updateTime = proc () -> do
  t <- S.run (const SDL.ticks) -< ()
  S.mapSingle Q.set -< Time t
  returnA -< ()

-- | Keyboard input component.
data KeyboardInput = KeyboardInput
  { -- | Keyboard events that occured this frame.
    keyboardEvents :: Map Keycode InputMotion,
    -- | Keys that are currently pressed.
    keyboardPressed :: Set Keycode
  }
  deriving (Show)

instance Component KeyboardInput

-- | @True@ if this key is currently pressed.
isKeyPressed :: Keycode -> KeyboardInput -> Bool
isKeyPressed key kb = Set.member key $ keyboardPressed kb

-- | Check for a key event that occured this frame.
keyEvent :: Keycode -> KeyboardInput -> Maybe InputMotion
keyEvent key kb = Map.lookup key $ keyboardEvents kb

-- | @True@ if this key was pressed this frame.
wasKeyPressed :: Keycode -> KeyboardInput -> Bool
wasKeyPressed key kb = case keyEvent key kb of
  Just Pressed -> True
  _ -> False

-- | @True@ if this key was released this frame.
wasKeyReleased :: Keycode -> KeyboardInput -> Bool
wasKeyReleased key kb = case keyEvent key kb of
  Just Released -> True
  _ -> False

-- | Mouse input component.
data MouseInput = MouseInput
  { -- | Mouse position in screen-space.
    mousePosition :: Point V2 Int,
    -- | Mouse offset since last frame.
    mouseOffset :: V2 Int,
    -- | Mouse button states.
    mouseButtons :: Map MouseButton InputMotion
  }
  deriving (Show)

instance Component MouseInput

-- | Keyboard input system.
handleInput :: System () ()
handleInput = proc () -> do
  events <- S.run . const $ SDL.pollEvents -< ()
  kb <- S.single Q.fetch -< ()
  mouseInput <- S.single Q.fetch -< ()
  let go (kbAcc, mouseAcc) event = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          ( KeyboardInput
              ( Map.insert
                  (keysymKeycode $ keyboardEventKeysym keyboardEvent)
                  (keyboardEventKeyMotion keyboardEvent)
                  (keyboardEvents kbAcc)
              )
              ( case keyboardEventKeyMotion keyboardEvent of
                  Pressed ->
                    Set.insert
                      (keysymKeycode $ keyboardEventKeysym keyboardEvent)
                      (keyboardPressed kbAcc)
                  Released ->
                    Set.delete
                      (keysymKeycode $ keyboardEventKeysym keyboardEvent)
                      (keyboardPressed kbAcc)
              ),
            mouseAcc
          )
        MouseMotionEvent mouseMotionEvent ->
          ( kbAcc,
            MouseInput
              { mousePosition = (fmap fromIntegral $ mouseMotionEventPos mouseMotionEvent),
                mouseOffset = (fmap fromIntegral $ mouseMotionEventRelMotion mouseMotionEvent),
                mouseButtons = (mouseButtons mouseAcc)
              }
          )
        MouseButtonEvent mouseButtonEvent ->
          ( kbAcc,
            MouseInput
              { mousePosition = (fmap fromIntegral $ mouseButtonEventPos mouseButtonEvent),
                mouseOffset = V2 0 0,
                mouseButtons =
                  Map.insert
                    (mouseButtonEventButton mouseButtonEvent)
                    (mouseButtonEventMotion mouseButtonEvent)
                    (mouseButtons mouseAcc)
              }
          )
        _ -> (kbAcc, mouseAcc)
      (kb', mouseInput') = foldl' go (kb, mouseInput) events
  S.mapSingle Q.set -< kb'
  S.mapSingle Q.set -< mouseInput'
  returnA -< ()

clearKeyboardQuery :: (Monad m) => Query m () KeyboardInput
clearKeyboardQuery = proc () -> do
  kb <- Q.fetch -< ()
  Q.set -< kb {keyboardEvents = mempty}

clearKeyboard :: System () ()
clearKeyboard = const () <$> S.mapSingle clearKeyboardQuery

clearMouseInputQuery :: (Monad m) => Query m () MouseInput
clearMouseInputQuery = proc () -> do
  mouseInput <- Q.fetch -< ()
  Q.set -< mouseInput {mouseButtons = mempty, mouseOffset = V2 0 0}

clearMouseInput :: System () ()
clearMouseInput = const () <$> S.mapSingle clearMouseInputQuery
