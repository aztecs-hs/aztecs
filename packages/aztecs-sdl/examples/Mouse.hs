{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (MouseInput, Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

update :: Schedule IO () ()
update = reader (S.all (Q.fetch @_ @MouseInput)) >>> task print

main :: IO ()
main = runSchedule_ $ SDL.setup >>> system setup >>> forever_ (SDL.update >>> update >>> SDL.draw)
