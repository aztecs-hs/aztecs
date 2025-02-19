{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Arrow ((>>>))
import Aztecs
import qualified Aztecs.ECS.Access as A
import Aztecs.ECS.SDL (Window (..))
import qualified Aztecs.ECS.SDL as SDL
import qualified Aztecs.ECS.System as S

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

main :: IO ()
main =
  runSchedule_ $
    SDL.setup
      >>> system setup
      >>> forever_ (SDL.update >>> SDL.draw)
