{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

app :: Access IO ()
app = do
  -- Spawn an entity with position and velocity components
  A.spawn_ (Position 0 :& Velocity 1)

  -- Update all matching entities
  q <- A.map (\(Position x :& Velocity v) -> Position (x + v))
  liftIO $ print q

main :: IO ()
main = do
  _ <- runAccess app W.empty
  return ()
