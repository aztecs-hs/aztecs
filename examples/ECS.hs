{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.System (System (..))
import qualified Data.Aztecs.System as S
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

data S

instance System IO S where
  edit = S.mapM (S.all @_ @'[Position]) print

main :: IO ()
main = do
  (_, w) <- runAccess app W.empty
  _ <- S.runSystem @_ @S w
  return ()
