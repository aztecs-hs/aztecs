{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.SDL (Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle Window {windowTitle = "Aztecs"}

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.forever (SDL.update >>> SDL.draw)
