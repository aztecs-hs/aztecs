{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.SDL
  ( Keyboard (..),
    SDLPlugin (..),
    sdlPlugin,
  )
where

import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Map (Map)
import qualified Data.Map as Map
import SDL

newtype Keyboard = Keyboard (Map Keycode InputMotion) deriving (Show)

instance Component Keyboard

setupKeyboard :: Access IO ()
setupKeyboard = do
  initializeAll
  _ <- createWindow "SDL" defaultWindow

  S.command $ do
    _ <- C.spawn $ Keyboard Map.empty
    return ()

updateKeyboard :: Access IO ()
updateKeyboard = do
  events <- pollEvents

  let f event (Keyboard kb) = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          let keyCode = keysymKeycode (keyboardEventKeysym keyboardEvent)
              motion = keyboardEventKeyMotion keyboardEvent
           in Keyboard $ Map.insert keyCode motion kb
        _ -> Keyboard kb
      g kb = foldr f kb events

  _ <- S.all $ do
    Q.write g

  return ()

data SDLPlugin = SDLPlugin
  { setupKeyboardId :: SystemId,
    updateKeyboardId :: SystemId
  }

sdlPlugin :: Scheduler IO SDLPlugin
sdlPlugin = do
  setupKeyboardId <- schedule Update [] setupKeyboard
  updateKeyboardId <- schedule Startup [] updateKeyboard
  return SDLPlugin {..}
