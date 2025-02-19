{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.SDL (MouseInput, Window (..))
import qualified Aztecs.ECS.SDL as SDL
import qualified Aztecs.ECS.System as S

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

update :: Schedule IO () ()
update = reader (S.all (Q.fetch @_ @MouseInput)) >>> task print

main :: IO ()
main = runSchedule_ $ SDL.setup >>> system setup >>> forever_ (SDL.update >>> update >>> SDL.draw)
