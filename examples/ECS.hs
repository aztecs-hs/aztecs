{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S

-- Components

data Position = Position Int deriving (Show)

instance Component Position

data Velocity = Velocity Int deriving (Show)

instance Component Velocity

-- Systems

data A

instance System IO A where
  access = S.command $ do
    e <- C.spawn (Position 0)
    C.insert e (Velocity 1)

data B

instance System IO B where
  access = do
    positions <- S.all $ do
      Velocity y <- Q.read
      Q.write (\(Position x) -> Position (x + y))

    liftIO $ print positions

app :: Scheduler IO
app = schedule @Startup @_ @A [] <> schedule @Update @_ @B []

main :: IO ()
main = runScheduler app
