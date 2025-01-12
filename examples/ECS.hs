{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs.Edit (Edit)
import qualified Data.Aztecs.Edit as C
import Data.Aztecs.Entity
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

app :: Edit IO ()
app = do
  C.spawn_ $ entity (Position 0) <&> Velocity 1
  C.spawn_ $ entity (Position 2) <&> Velocity 2

  positions <- Q.map $
    \(Position p :& Velocity v) -> Position (p + v)

  liftIO $ pPrint positions

main :: IO ()
main = do
  _ <- C.runEdit app W.empty
  return ()
