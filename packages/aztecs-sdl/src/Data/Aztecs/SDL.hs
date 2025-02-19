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

    -- * Surface components
    Surface (..),
    SurfaceTarget (..),
    SurfaceTexture (..),

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
    buildTextures,
    drawTextures,
    addCameraTargets,
    addSurfaceTargets,
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
import Data.Aztecs.Query (ArrowQuery)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.Query.Reader (QueryReader)
import Data.Aztecs.System (ArrowReaderSystem)
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Size (..), Transform (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word32)
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

-- | Window component.
data Window = Window
  { -- | Window title.
    windowTitle :: !String
  }
  deriving (Show)

instance Component Window

-- | Window renderer component.
data WindowRenderer = WindowRenderer
  { -- | SDL window.
    windowRendererRaw :: !SDL.Window,
    -- | SDL renderer.
    windowRenderer :: !Renderer
  }
  deriving (Show)

instance Component WindowRenderer

-- | Camera component.
data Camera = Camera
  { -- | Camera viewport size.
    cameraViewport :: !(V2 Int),
    -- | Camera scale factor.
    cameraScale :: !(V2 Float)
  }
  deriving (Show)

instance Component Camera

-- | Camera target component.
newtype CameraTarget = CameraTarget
  { -- | This camera's target window.
    cameraTargetWindow :: EntityID
  }
  deriving (Eq, Show)

instance Component CameraTarget

-- | Setup SDL
setup :: Schedule IO () ()
setup =
  fmap (const ()) $
    task (const initializeAll)
      &&& system
        ( S.queue
            ( const $ do
                A.spawn_ . bundle $ Time 0
                A.spawn_ . bundle $ KeyboardInput mempty mempty
                A.spawn_ . bundle $ MouseInput (P $ V2 0 0) (V2 0 0) mempty
            )
        )

-- | Update SDL windows
update :: Schedule IO () ()
update =
  updateTime
    >>> addWindows
    >>> system (addCameraTargets >>> addSurfaceTargets)
    >>> buildTextures
    >>> handleInput

-- | Draw to SDL windows and clear input events.
draw :: Schedule IO () ()
draw = drawTextures >>> system clearInput

-- | Setup new windows.
addWindows :: Schedule IO () ()
addWindows = proc () -> do
  newWindows <- reader $ S.filter (Q.entity &&& Q.fetch @_ @Window) (without @WindowRenderer) -< ()
  newWindows' <- task $ mapM createWindowRenderer -< newWindows
  system $ S.queue $ mapM_ insertWindowRenderer -< newWindows'
  where
    createWindowRenderer (eId, window) = do
      sdlWindow <- createWindow (T.pack $ windowTitle window) defaultWindow
      renderer <- createRenderer sdlWindow (-1) defaultRenderer
      return (eId, sdlWindow, renderer)
    insertWindowRenderer (eId, window, renderer) = A.insert eId (WindowRenderer window renderer)

-- | Surface texture component.
newtype SurfaceTexture = SurfaceTexture
  { -- | SDL texture.
    unSurfaceTexture :: SDL.Texture
  }

instance Component SurfaceTexture

allWindowTextures ::
  (ArrowReaderSystem arr) =>
  arr () [(WindowRenderer, [(EntityID, Surface, Transform, Maybe SurfaceTexture)])]
allWindowTextures =
  allWindowDraws
    (pure ())
    ( proc () -> do
        e <- Q.entity -< ()
        surface <- Q.fetch -< ()
        transform <- Q.fetch -< ()
        texture <- Q.fetchMaybe -< ()
        returnA -< (e, surface, transform, texture)
    )
    >>> arr (\cs -> map (\(w, cs') -> (w, concatMap snd cs')) cs)

-- | Build textures from surfaces in preparation for `drawTextures`.
buildTextures :: Schedule IO () ()
buildTextures =
  let go windowDraws =
        mapM_
          ( \(window, cameraDraws) -> do
              let renderer = windowRenderer window
              mapM_
                ( \(eId, surface, transform, maybeTexture) -> do
                    sdlTexture <- SDL.createTextureFromSurface renderer $ sdlSurface surface
                    textureDesc <- queryTexture sdlTexture
                    case maybeTexture of
                      Just (SurfaceTexture lastTexture) -> destroyTexture lastTexture
                      Nothing -> return ()
                    A.insert eId (SurfaceTexture sdlTexture)
                    A.insert
                      eId
                      ( Size $
                          transformScale transform
                            * fromMaybe
                              (fmap fromIntegral $ V2 (textureWidth textureDesc) (textureHeight textureDesc))
                              ((fmap (\(Rectangle _ s) -> fmap fromIntegral s) $ surfaceBounds surface))
                      )
                )
                cameraDraws
          )
          windowDraws
   in reader allWindowTextures >>> access go

drawTextures :: Schedule IO () ()
drawTextures =
  let go windowDraws =
        mapM_
          ( \(window, cameraDraws) -> do
              mapM_
                ( \((camera, cameraTransform), cameraDraws') -> do
                    let renderer = windowRenderer window
                    rendererDrawColor renderer $= V4 0 0 0 255
                    rendererScale renderer $= fmap realToFrac (cameraScale camera)
                    rendererViewport renderer
                      $= Just
                        ( Rectangle
                            (P (fmap fromIntegral $ transformPosition cameraTransform))
                            (fmap fromIntegral $ cameraViewport camera)
                        )
                    clear renderer
                    mapM_
                      ( \(surface, transform, texture) -> do
                          textureDesc <- queryTexture $ unSurfaceTexture texture
                          copyEx
                            renderer
                            (unSurfaceTexture texture)
                            (fmap fromIntegral <$> surfaceBounds surface)
                            ( Just
                                ( Rectangle
                                    (fmap fromIntegral . P $ transformPosition transform)
                                    ( fromMaybe
                                        (fmap fromIntegral $ V2 (textureWidth textureDesc) (textureHeight textureDesc))
                                        ((fmap (\(Rectangle _ s) -> fmap fromIntegral s) $ surfaceBounds surface))
                                    )
                                )
                            )
                            (realToFrac $ transformRotation transform)
                            Nothing
                            (V2 False False)
                      )
                      cameraDraws'
                    present renderer
                )
                cameraDraws
          )
          windowDraws
   in reader allCameraSurfaces >>> access go

allCameraSurfaces ::
  (ArrowReaderSystem arr) =>
  arr () [(WindowRenderer, [((Camera, Transform), [(Surface, Transform, SurfaceTexture)])])]
allCameraSurfaces =
  allWindowDraws
    (Q.fetch &&& Q.fetch)
    ( proc () -> do
        surface <- Q.fetch -< ()
        transform <- Q.fetch -< ()
        texture <- Q.fetch -< ()
        returnA -< (surface, transform, texture)
    )

allWindowDraws ::
  (ArrowReaderSystem arr) =>
  (QueryReader () a) ->
  (QueryReader () b) ->
  arr () [(WindowRenderer, [(a, [b])])]
allWindowDraws qA qB = proc () -> do
  cameras <-
    S.all
      ( proc () -> do
          eId <- Q.entity -< ()
          cameraTarget <- Q.fetch @_ @CameraTarget -< ()
          a <- qA -< ()
          returnA -< (eId, cameraTarget, a)
      )
      -<
        ()
  windows <- S.all (Q.entity &&& Q.fetch @_ @WindowRenderer) -< ()
  draws <-
    S.all
      ( proc () -> do
          t <- Q.fetch @_ @SurfaceTarget -< ()
          a <- qB -< ()
          returnA -< (t, a)
      )
      -<
        ()
  let cameraDraws =
        map
          ( \(eId, cameraTarget, b) ->
              ( cameraTarget,
                b,
                mapMaybe
                  ( \(surfaceTarget, a) ->
                      if drawTargetCamera surfaceTarget == eId
                        then Just a
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
                map (\(_, a, b) -> (a, b)) $
                  filter (\(cameraTarget, _, _) -> cameraTargetWindow cameraTarget == eId) cameraDraws
              )
          )
          windows
  returnA -< windowDraws

-- | Surface target component.
-- This component can be used to specify which `Camera` to draw a `Surface` to.
newtype SurfaceTarget = SurfaceTarget {drawTargetCamera :: EntityID}
  deriving (Eq, Show)

instance Component SurfaceTarget

-- | Surface component.
-- This component can be used to draw to a window.
data Surface = Surface
  { sdlSurface :: !SDL.Surface,
    surfaceBounds :: !(Maybe (Rectangle Int))
  }

instance Component Surface

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

-- | Add `SurfaceTarget` components to entities with a new `Surface` component.
addSurfaceTargets :: System () ()
addSurfaceTargets = proc () -> do
  cameras <- S.all (Q.entity &&& Q.fetch @_ @Camera) -< ()
  newDraws <- S.filter (Q.entity &&& Q.fetch @_ @Surface) (without @SurfaceTarget) -< ()
  S.queue
    ( \(newDraws, cameras) -> case cameras of
        (cameraEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId $ SurfaceTarget cameraEId) newDraws
        _ -> return ()
    )
    -<
      (newDraws, cameras)

newtype Time = Time {elapsedMS :: Word32}
  deriving (Eq, Ord, Num, Show)

instance Component Time

updateTime :: Schedule IO () ()
updateTime = proc () -> do
  t <- task (const SDL.ticks) -< ()
  system $ S.mapSingle Q.set -< Time t
  returnA -< ()

-- | Keyboard input component.
data KeyboardInput = KeyboardInput
  { -- | Keyboard events that occured this frame.
    keyboardEvents :: !(Map Keycode InputMotion),
    -- | Keys that are currently pressed.
    keyboardPressed :: !(Set Keycode)
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
    mousePosition :: !(Point V2 Int),
    -- | Mouse offset since last frame.
    mouseOffset :: !(V2 Int),
    -- | Mouse button states.
    mouseButtons :: !(Map MouseButton InputMotion)
  }
  deriving (Show)

instance Component MouseInput

handleInput :: Schedule IO () ()
handleInput = task (const pollEvents) >>> system handleInput'

-- | Keyboard input system.
handleInput' :: System [Event] ()
handleInput' = proc events -> do
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

clearInput :: System () ()
clearInput = const () <$> (clearKeyboard &&& clearMouseInput)

clearKeyboardQuery :: (ArrowQuery arr) => arr () KeyboardInput
clearKeyboardQuery = proc () -> do
  kb <- Q.fetch -< ()
  Q.set -< kb {keyboardEvents = mempty}

clearKeyboard :: System () ()
clearKeyboard = const () <$> S.mapSingle clearKeyboardQuery

clearMouseInputQuery :: (ArrowQuery arr) => arr () MouseInput
clearMouseInputQuery = proc () -> do
  mouseInput <- Q.fetch -< ()
  Q.set -< mouseInput {mouseButtons = mempty, mouseOffset = V2 0 0}

clearMouseInput :: System () ()
clearMouseInput = const () <$> S.mapSingle clearMouseInputQuery
