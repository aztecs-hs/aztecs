{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL
import qualified Data.Aztecs.Task as T

data A = A (Query Keyboard)

instance System IO A where
  access = A <$> query Q.read
  run (A keys) = do
    allKeys <- T.all keys
    liftIO $ print allKeys

app :: Scheduler IO
app = sdlPlugin <> schedule @Update @_ @A []

main :: IO ()
main = runScheduler app
