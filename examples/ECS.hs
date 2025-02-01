{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Scheduler (Startup, Update, schedule)
import qualified Data.Aztecs.System as S

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data Setup

instance System IO Setup where
  task = S.queue (A.spawn_ (Position 0 :& Velocity 1))

data Movement

instance System IO Movement where
  task = S.map (\(Position x :& Velocity v) -> Position (x + v)) >>> S.run print

main :: IO ()
main = do
  let s = schedule @IO @Startup @Setup [] <> schedule @_ @Update @Movement []
  print s
