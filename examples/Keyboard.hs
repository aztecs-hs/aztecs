{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.SDL as SDL
import Control.Arrow ((>>>))
import Control.Monad (when)
import Control.Monad.IO.Class

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

update :: Schedule IO () ()
update =
  reader (S.single (Q.fetch @_ @KeyboardInput))
    >>> access
      ( \kb -> liftIO $ do
          when (wasKeyPressed KeyW kb) $ print "Onwards!"
          when (wasKeyPressed KeyS kb) $ print "Retreat..."
          when (wasKeyReleased KeyW kb || wasKeyReleased KeyS kb) $ print "Halt!"
      )

main :: IO ()
main =
  runSchedule_ $
    SDL.setup >>> system setup >>> forever_ (SDL.update >>> update >>> SDL.draw)
