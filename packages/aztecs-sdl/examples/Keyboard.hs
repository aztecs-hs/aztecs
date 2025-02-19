{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.SDL (KeyboardInput, Window (..))
import qualified Aztecs.ECS.SDL as SDL
import qualified Aztecs.ECS.System as S
import Control.Arrow ((>>>))

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

update :: System () ()
update = S.all (Q.fetch @_ @KeyboardInput) >>> S.task print

main :: IO ()
main = runSchedule_ $ SDL.setup >>> system setup >>> forever_ (SDL.update >>> update >>> SDL.draw)
