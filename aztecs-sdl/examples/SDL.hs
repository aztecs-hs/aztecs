{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL
import qualified Data.Aztecs.System as S

run :: Access IO ()
run = do
  keys <- S.all (Q.read @_ @Keyboard)
  liftIO $ print keys

app :: Scheduler IO ()
app = do
  _ <- sdlPlugin
  _ <- schedule Update [] run
  return ()

main :: IO ()
main = runScheduler app
