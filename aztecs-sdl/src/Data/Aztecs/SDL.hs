{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.SDL
  ( Keyboard (..),
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

data SetupKeyboard

instance System IO SetupKeyboard where
  access = do
    initializeAll
    _ <- createWindow "SDL" defaultWindow

    S.command $ do
      _ <- C.spawn $ Keyboard Map.empty
      return ()

data KeyboardSystem

instance System IO KeyboardSystem where
  access = do
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

sdlPlugin :: Scheduler IO
sdlPlugin = schedule @Startup @_ @SetupKeyboard [] <> schedule @Update @_ @KeyboardSystem []
