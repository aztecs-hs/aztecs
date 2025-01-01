{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL
import qualified Data.Aztecs.System as S

data A

instance System IO A where
  access = do
    keys <- S.all (Q.read @Keyboard)
    liftIO $ print keys

app :: Scheduler IO
app = sdlPlugin <> schedule @Update @_ @A []

main :: IO ()
main = runScheduler app
