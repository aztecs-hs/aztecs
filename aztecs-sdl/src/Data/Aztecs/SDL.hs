{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.SDL
  ( Keyboard (..),
    SetupKeyboard (..),
    KeyboardSystem (..),
    sdlPlugin,
  )
where

import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.Task as T
import Data.Map (Map)
import qualified Data.Map as Map
import SDL

newtype Keyboard = Keyboard (Map Keycode InputMotion) deriving (Show)

instance Component Keyboard

data SetupKeyboard = SetupKeyboard

instance System IO SetupKeyboard where
  access = pure SetupKeyboard
  run SetupKeyboard = do
    initializeAll
    _ <- createWindow "SDL" defaultWindow

    T.command $ do
      _ <- C.spawn $ Keyboard Map.empty
      return ()

data KeyboardSystem = KeyboardSystem (Query (Write Keyboard))

instance System IO KeyboardSystem where
  access = KeyboardSystem <$> query Q.write
  run (KeyboardSystem keys) = do
    events <- pollEvents

    allKeys <- T.all keys

    let f event acc = case eventPayload event of
          KeyboardEvent keyboardEvent ->
            let keyCode = keysymKeycode (keyboardEventKeysym keyboardEvent)
                motion = keyboardEventKeyMotion keyboardEvent
             in Map.insert keyCode motion acc
          _ -> acc
        (QueryResult _ writes) = allKeys
        (Keyboard keys') = map Q.unWrite writes !! 0
        keys'' = foldr f keys' events

    T.alter allKeys (const (Keyboard keys''))

sdlPlugin :: Scheduler IO
sdlPlugin =
  schedule @Startup @_ @SetupKeyboard []
    <> schedule @Update @_ @KeyboardSystem []
