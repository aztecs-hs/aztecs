module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.System as S
import qualified Aztecs.SDL as SDL
import Control.Arrow ((>>>))

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

main :: IO ()
main = runSchedule_ $ SDL.setup >>> system setup >>> forever_ (SDL.update >>> SDL.draw)
