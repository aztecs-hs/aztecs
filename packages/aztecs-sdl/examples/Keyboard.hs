{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (KeyboardInput, Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

update :: System () ()
update = S.all (Q.fetch @_ @KeyboardInput) >>> S.task print

main :: IO ()
main =
  runSchedule_ $
    schedule SDL.setup
      >>> schedule setup
      >>> forever (schedule SDL.update >>> schedule update >>> schedule SDL.draw)
