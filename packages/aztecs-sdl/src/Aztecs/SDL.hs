{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.SDL
  ( -- * Window components
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
    fromSDLKeycode,
    toSDLKeycode,

    -- ** Mouse input
    mouseButtonFromSDL,
    mouseButtonToSDL,
    motionFromSDL,
    motionToSDL,

    -- * Systems
    setup,
    update,
    draw,

    -- ** Primitive systems
    addWindows,
    buildTextures,
    addSurfaceTargets,
    handleInput,
    updateTime,
  )
where

import Aztecs
import Aztecs.Camera (addCameraTargets)
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.Input
  ( InputMotion (..),
    MouseButton (..),
    handleKeyboardEvent,
    keyboardInput,
    mouseInput,
  )
import Control.Arrow (Arrow (..), returnA, (>>>))
import Control.DeepSeq
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import SDL hiding (InputMotion (..), MouseButton (..), Surface, Texture, Window, windowTitle)
import qualified SDL

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

-- | Window renderer component.
data WindowRenderer = WindowRenderer
  { -- | SDL window.
    windowRendererRaw :: !SDL.Window,
    -- | SDL renderer.
    windowRenderer :: !Renderer
  }
  deriving (Show, Generic)

instance Component WindowRenderer

instance NFData WindowRenderer where
  rnf = rwhnf

-- | Setup SDL
setup :: (MonadIO m) => (ArrowAccessSchedule b m arr) => arr () ()
setup =
  access (const $ liftIO initializeAll)
    >>> access
      ( const $ do
          A.spawn_ . bundle $ Time 0
          A.spawn_ $ bundle keyboardInput
          A.spawn_ $ bundle mouseInput
      )

-- | Update SDL windows
update ::
  ( ArrowQueryReader qr,
    ArrowReaderSystem qr rs,
    ArrowQueueSystem b qm rs,
    ArrowReaderSchedule rs arr,
    ArrowQuery q,
    ArrowSystem q s,
    ArrowReaderSystem qr s,
    ArrowQueueSystem b qm s,
    ArrowSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
update =
  updateTime
    >>> addWindows
    >>> reader (addCameraTargets >>> addSurfaceTargets)
    >>> buildTextures
    >>> handleInput

-- | Setup new windows.
addWindows ::
  ( ArrowQueryReader q,
    ArrowReaderSystem q s,
    ArrowQueueSystem b qm s,
    ArrowReaderSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
addWindows = proc () -> do
  newWindows <- reader $ S.filter (Q.entity &&& Q.fetch @_ @Window) (without @WindowRenderer) -< ()
  newWindows' <- access $ liftIO . mapM createWindowRenderer -< newWindows
  reader $ S.queue $ mapM_ insertWindowRenderer -< newWindows'
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
  deriving (Generic)

instance Component SurfaceTexture

instance NFData SurfaceTexture where
  rnf = rwhnf

allWindowTextures ::
  (ArrowQueryReader q, ArrowReaderSystem q arr) =>
  arr () [(WindowRenderer, [(EntityID, Surface, Transform2D, Maybe SurfaceTexture)])]
allWindowTextures =
  allWindowDraws
    (arr (const ()))
    ( proc () -> do
        e <- Q.entity -< ()
        surface <- Q.fetch -< ()
        transform <- Q.fetch -< ()
        texture <- Q.fetchMaybe -< ()
        returnA -< (e, surface, transform, texture)
    )
    >>> arr (\cs -> map (\(w, cs') -> (w, concatMap snd cs')) cs)

-- | Build textures from surfaces in preparation for `drawTextures`.
buildTextures ::
  ( ArrowQueryReader q,
    ArrowReaderSystem q s,
    ArrowReaderSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
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

draw ::
  ( ArrowQueryReader q,
    ArrowReaderSystem q s,
    ArrowReaderSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
draw =
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
                            (P (fmap fromIntegral $ transformTranslation cameraTransform))
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
                                    (fmap fromIntegral . P $ transformTranslation transform)
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
  (ArrowQueryReader q, ArrowReaderSystem q arr) =>
  arr () [(WindowRenderer, [((Camera, Transform2D), [(Surface, Transform2D, SurfaceTexture)])])]
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
  (ArrowQueryReader q, ArrowReaderSystem q arr) =>
  (q () a) ->
  (q () b) ->
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
  deriving (Eq, Show, Generic, NFData)

instance Component SurfaceTarget

-- | Surface component.
-- This component can be used to draw to a window.
data Surface = Surface
  { sdlSurface :: !SDL.Surface,
    surfaceBounds :: !(Maybe (Rectangle Int))
  }

instance Component Surface

instance NFData Surface where
  rnf = rwhnf

-- | Add `SurfaceTarget` components to entities with a new `Surface` component.
addSurfaceTargets ::
  (ArrowQueryReader q, ArrowReaderSystem q arr, ArrowQueueSystem b m arr) => arr () ()
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

updateTime ::
  ( ArrowQuery q,
    ArrowSystem q s,
    ArrowSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
updateTime = proc () -> do
  t <- access . const $ liftIO SDL.ticks -< ()
  system $ S.mapSingle Q.set -< Time t
  returnA -< ()

handleInput ::
  ( ArrowQueryReader qr,
    ArrowReaderSystem qr s,
    ArrowQuery q,
    ArrowSystem q s,
    ArrowSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
handleInput = access (const pollEvents) >>> system handleInput'

-- | Keyboard input system.
handleInput' ::
  (ArrowQueryReader qr, ArrowReaderSystem qr arr, ArrowQuery q, ArrowSystem q arr) =>
  arr [Event] ()
handleInput' = proc events -> do
  kb <- S.single Q.fetch -< ()
  mouse <- S.single Q.fetch -< ()
  let go (kbAcc, mouseAcc) event = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          ( case fromSDLKeycode $ keysymKeycode $ keyboardEventKeysym keyboardEvent of
              Just key -> handleKeyboardEvent key (motionFromSDL $ keyboardEventKeyMotion keyboardEvent) kbAcc
              Nothing -> kbAcc,
            mouseAcc
          )
        MouseMotionEvent mouseMotionEvent ->
          ( kbAcc,
            mouseAcc
              { mousePosition = (fmap fromIntegral $ mouseMotionEventPos mouseMotionEvent),
                mouseOffset = (fmap fromIntegral $ mouseMotionEventRelMotion mouseMotionEvent),
                mouseButtons = (mouseButtons mouseAcc)
              }
          )
        MouseButtonEvent mouseButtonEvent ->
          ( kbAcc,
            mouseAcc
              { mousePosition = (fmap fromIntegral $ mouseButtonEventPos mouseButtonEvent),
                mouseButtons =
                  Map.insert
                    (mouseButtonFromSDL $ mouseButtonEventButton mouseButtonEvent)
                    (motionFromSDL $ mouseButtonEventMotion mouseButtonEvent)
                    (mouseButtons mouseAcc)
              }
          )
        _ -> (kbAcc, mouseAcc)
      (kb', mouseInput') = foldl' go (kb {keyboardEvents = mempty}, mouse {mouseOffset = V2 0 0}) events
  S.mapSingle Q.set -< kb'
  S.mapSingle Q.set -< mouseInput'
  returnA -< ()

mouseButtonFromSDL :: SDL.MouseButton -> MouseButton
mouseButtonFromSDL b = case b of
  SDL.ButtonLeft -> ButtonLeft
  SDL.ButtonMiddle -> ButtonMiddle
  SDL.ButtonRight -> ButtonRight
  SDL.ButtonX1 -> ButtonX1
  SDL.ButtonX2 -> ButtonX2
  SDL.ButtonExtra i -> ButtonExtra i

mouseButtonToSDL :: MouseButton -> SDL.MouseButton
mouseButtonToSDL b = case b of
  ButtonLeft -> SDL.ButtonLeft
  ButtonMiddle -> SDL.ButtonMiddle
  ButtonRight -> SDL.ButtonRight
  ButtonX1 -> SDL.ButtonX1
  ButtonX2 -> SDL.ButtonX2
  ButtonExtra i -> SDL.ButtonExtra i

motionFromSDL :: SDL.InputMotion -> InputMotion
motionFromSDL m = case m of
  SDL.Pressed -> Pressed
  SDL.Released -> Released

motionToSDL :: InputMotion -> SDL.InputMotion
motionToSDL m = case m of
  Pressed -> SDL.Pressed
  Released -> SDL.Released

toSDLKeycode :: Key -> SDL.Keycode
toSDLKeycode key = case key of
  KeyA -> SDL.KeycodeA
  KeyB -> SDL.KeycodeB
  KeyC -> SDL.KeycodeC
  KeyD -> SDL.KeycodeD
  KeyE -> SDL.KeycodeE
  KeyF -> SDL.KeycodeF
  KeyG -> SDL.KeycodeG
  KeyH -> SDL.KeycodeH
  KeyI -> SDL.KeycodeI
  KeyJ -> SDL.KeycodeJ
  KeyK -> SDL.KeycodeK
  KeyL -> SDL.KeycodeL
  KeyM -> SDL.KeycodeM
  KeyN -> SDL.KeycodeN
  KeyO -> SDL.KeycodeO
  KeyP -> SDL.KeycodeP
  KeyQ -> SDL.KeycodeQ
  KeyR -> SDL.KeycodeR
  KeyS -> SDL.KeycodeS
  KeyT -> SDL.KeycodeT
  KeyU -> SDL.KeycodeU
  KeyV -> SDL.KeycodeV
  KeyW -> SDL.KeycodeW
  KeyX -> SDL.KeycodeX
  KeyY -> SDL.KeycodeY
  KeyZ -> SDL.KeycodeZ
  Key0 -> SDL.Keycode0
  Key1 -> SDL.Keycode1
  Key2 -> SDL.Keycode2
  Key3 -> SDL.Keycode3
  Key4 -> SDL.Keycode4
  Key5 -> SDL.Keycode5
  Key6 -> SDL.Keycode6
  Key7 -> SDL.Keycode7
  Key8 -> SDL.Keycode8
  Key9 -> SDL.Keycode9
  KeyF1 -> SDL.KeycodeF1
  KeyF2 -> SDL.KeycodeF2
  KeyF3 -> SDL.KeycodeF3
  KeyF4 -> SDL.KeycodeF4
  KeyF5 -> SDL.KeycodeF5
  KeyF6 -> SDL.KeycodeF6
  KeyF7 -> SDL.KeycodeF7
  KeyF8 -> SDL.KeycodeF8
  KeyF9 -> SDL.KeycodeF9
  KeyF10 -> SDL.KeycodeF10
  KeyF11 -> SDL.KeycodeF11
  KeyF12 -> SDL.KeycodeF12
  KeyEscape -> SDL.KeycodeEscape
  KeyEnter -> SDL.KeycodeReturn
  KeySpace -> SDL.KeycodeSpace
  KeyBackspace -> SDL.KeycodeBackspace
  KeyTab -> SDL.KeycodeTab
  KeyCapsLock -> SDL.KeycodeCapsLock
  KeyShift -> SDL.KeycodeLShift
  KeyCtrl -> SDL.KeycodeLCtrl
  KeyAlt -> SDL.KeycodeLAlt
  KeyLeft -> SDL.KeycodeLeft
  KeyRight -> SDL.KeycodeRight
  KeyUp -> SDL.KeycodeUp
  KeyDown -> SDL.KeycodeDown
  KeyHome -> SDL.KeycodeHome
  KeyEnd -> SDL.KeycodeEnd
  KeyPageUp -> SDL.KeycodePageUp
  KeyPageDown -> SDL.KeycodePageDown
  KeyInsert -> SDL.KeycodeInsert
  KeyDelete -> SDL.KeycodeDelete
  KeyMinus -> SDL.KeycodeMinus
  KeyEquals -> SDL.KeycodeEquals
  KeyBracketLeft -> SDL.KeycodeLeftBracket
  KeyBracketRight -> SDL.KeycodeRightBracket
  KeyBackslash -> SDL.KeycodeBackslash
  KeySemicolon -> SDL.KeycodeSemicolon
  KeyComma -> SDL.KeycodeComma
  KeyPeriod -> SDL.KeycodePeriod
  KeySlash -> SDL.KeycodeSlash
  KeyNumLock -> SDL.KeycodeNumLockClear
  KeyNumpad0 -> SDL.KeycodeKP0
  KeyNumpad1 -> SDL.KeycodeKP1
  KeyNumpad2 -> SDL.KeycodeKP2
  KeyNumpad3 -> SDL.KeycodeKP3
  KeyNumpad4 -> SDL.KeycodeKP4
  KeyNumpad5 -> SDL.KeycodeKP5
  KeyNumpad6 -> SDL.KeycodeKP6
  KeyNumpad7 -> SDL.KeycodeKP7
  KeyNumpad8 -> SDL.KeycodeKP8
  KeyNumpad9 -> SDL.KeycodeKP9
  KeyNumpadDivide -> SDL.KeycodeKPDivide
  KeyNumpadMultiply -> SDL.KeycodeKPMultiply
  KeyNumpadMinus -> SDL.KeycodeKPMinus
  KeyNumpadPlus -> SDL.KeycodeKPPlus
  KeyNumpadEnter -> SDL.KeycodeKPEnter
  KeyNumpadPeriod -> SDL.KeycodeKPPeriod
  KeySuper -> SDL.KeycodeLGUI -- assuming left Super (Windows/Command key); SDL also has RGUI
  KeyMenu -> SDL.KeycodeMenu

fromSDLKeycode :: SDL.Keycode -> Maybe Key
fromSDLKeycode keycode = case keycode of
  SDL.KeycodeA -> Just KeyA
  SDL.KeycodeB -> Just KeyB
  SDL.KeycodeC -> Just KeyC
  SDL.KeycodeD -> Just KeyD
  SDL.KeycodeE -> Just KeyE
  SDL.KeycodeF -> Just KeyF
  SDL.KeycodeG -> Just KeyG
  SDL.KeycodeH -> Just KeyH
  SDL.KeycodeI -> Just KeyI
  SDL.KeycodeJ -> Just KeyJ
  SDL.KeycodeK -> Just KeyK
  SDL.KeycodeL -> Just KeyL
  SDL.KeycodeM -> Just KeyM
  SDL.KeycodeN -> Just KeyN
  SDL.KeycodeO -> Just KeyO
  SDL.KeycodeP -> Just KeyP
  SDL.KeycodeQ -> Just KeyQ
  SDL.KeycodeR -> Just KeyR
  SDL.KeycodeS -> Just KeyS
  SDL.KeycodeT -> Just KeyT
  SDL.KeycodeU -> Just KeyU
  SDL.KeycodeV -> Just KeyV
  SDL.KeycodeW -> Just KeyW
  SDL.KeycodeX -> Just KeyX
  SDL.KeycodeY -> Just KeyY
  SDL.KeycodeZ -> Just KeyZ
  SDL.Keycode0 -> Just Key0
  SDL.Keycode1 -> Just Key1
  SDL.Keycode2 -> Just Key2
  SDL.Keycode3 -> Just Key3
  SDL.Keycode4 -> Just Key4
  SDL.Keycode5 -> Just Key5
  SDL.Keycode6 -> Just Key6
  SDL.Keycode7 -> Just Key7
  SDL.Keycode8 -> Just Key8
  SDL.Keycode9 -> Just Key9
  SDL.KeycodeF1 -> Just KeyF1
  SDL.KeycodeF2 -> Just KeyF2
  SDL.KeycodeF3 -> Just KeyF3
  SDL.KeycodeF4 -> Just KeyF4
  SDL.KeycodeF5 -> Just KeyF5
  SDL.KeycodeF6 -> Just KeyF6
  SDL.KeycodeF7 -> Just KeyF7
  SDL.KeycodeF8 -> Just KeyF8
  SDL.KeycodeF9 -> Just KeyF9
  SDL.KeycodeF10 -> Just KeyF10
  SDL.KeycodeF11 -> Just KeyF11
  SDL.KeycodeF12 -> Just KeyF12
  SDL.KeycodeEscape -> Just KeyEscape
  SDL.KeycodeReturn -> Just KeyEnter
  SDL.KeycodeSpace -> Just KeySpace
  SDL.KeycodeBackspace -> Just KeyBackspace
  SDL.KeycodeTab -> Just KeyTab
  SDL.KeycodeCapsLock -> Just KeyCapsLock
  SDL.KeycodeLShift -> Just KeyShift
  SDL.KeycodeRShift -> Just KeyShift
  SDL.KeycodeLCtrl -> Just KeyCtrl
  SDL.KeycodeRCtrl -> Just KeyCtrl
  SDL.KeycodeLAlt -> Just KeyAlt
  SDL.KeycodeRAlt -> Just KeyAlt
  SDL.KeycodeLeft -> Just KeyLeft
  SDL.KeycodeRight -> Just KeyRight
  SDL.KeycodeUp -> Just KeyUp
  SDL.KeycodeDown -> Just KeyDown
  SDL.KeycodeHome -> Just KeyHome
  SDL.KeycodeEnd -> Just KeyEnd
  SDL.KeycodePageUp -> Just KeyPageUp
  SDL.KeycodePageDown -> Just KeyPageDown
  SDL.KeycodeInsert -> Just KeyInsert
  SDL.KeycodeDelete -> Just KeyDelete
  SDL.KeycodeMinus -> Just KeyMinus
  SDL.KeycodeEquals -> Just KeyEquals
  SDL.KeycodeLeftBracket -> Just KeyBracketLeft
  SDL.KeycodeRightBracket -> Just KeyBracketRight
  SDL.KeycodeBackslash -> Just KeyBackslash
  SDL.KeycodeSemicolon -> Just KeySemicolon
  SDL.KeycodeComma -> Just KeyComma
  SDL.KeycodePeriod -> Just KeyPeriod
  SDL.KeycodeSlash -> Just KeySlash
  SDL.KeycodeNumLockClear -> Just KeyNumLock
  SDL.KeycodeKP0 -> Just KeyNumpad0
  SDL.KeycodeKP1 -> Just KeyNumpad1
  SDL.KeycodeKP2 -> Just KeyNumpad2
  SDL.KeycodeKP3 -> Just KeyNumpad3
  SDL.KeycodeKP4 -> Just KeyNumpad4
  SDL.KeycodeKP5 -> Just KeyNumpad5
  SDL.KeycodeKP6 -> Just KeyNumpad6
  SDL.KeycodeKP7 -> Just KeyNumpad7
  SDL.KeycodeKP8 -> Just KeyNumpad8
  SDL.KeycodeKP9 -> Just KeyNumpad9
  SDL.KeycodeKPDivide -> Just KeyNumpadDivide
  SDL.KeycodeKPMultiply -> Just KeyNumpadMultiply
  SDL.KeycodeKPMinus -> Just KeyNumpadMinus
  SDL.KeycodeKPPlus -> Just KeyNumpadPlus
  SDL.KeycodeKPEnter -> Just KeyNumpadEnter
  SDL.KeycodeKPPeriod -> Just KeyNumpadPeriod
  SDL.KeycodeLGUI -> Just KeySuper
  SDL.KeycodeRGUI -> Just KeySuper
  SDL.KeycodeMenu -> Just KeyMenu
  _ -> Nothing
