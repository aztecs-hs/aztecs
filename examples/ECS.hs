{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S

-- Components

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

-- Systems

setup :: System IO ()
setup = S.command $ do
  e <- C.spawn (Position 0)
  C.insert e (Velocity 1)

run :: System IO ()
run = do
  -- Update all entities with a `Position` and `Velocity` component.
  positions <- S.all $ do
    Velocity v <- Q.read
    Q.write (\(Position p) -> Position (p + v))

  liftIO $ print positions

app :: Scheduler IO ()
app = do
  _ <- schedule Startup [] setup
  _ <- schedule Update [] run
  return ()

main :: IO ()
main = runScheduler app
