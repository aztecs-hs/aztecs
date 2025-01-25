{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

app :: Access IO ()
app = do
  -- Spawn an entity with position and velocity components
  e <- A.spawn (Position 0)
  A.insert e (Velocity 1)

  -- Query for and update all matching entities
  q <- Q.map (\(Velocity v :& Position x) -> Position (x + v))
  liftIO $ print q

  x <- Q.all @_ @Position
  liftIO $ print x

main :: IO ()
main = do
  _ <- runAccess app W.empty
  return ()
