{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs.Command (Command)
import qualified Data.Aztecs.Command as C
import Data.Aztecs.Entity (component, entity, (<&>))
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

app :: Command IO ()
app = do
  C.spawn_ $ entity (Position 0) <&> Velocity 1
  C.spawn_ $ entity (Position 2) <&> Velocity 2

  positions <- Q.map @'[Position, Velocity] $ \e ->
    let (Position p) = component e
        (Velocity v) = component e
     in entity $ Position (p + v)

  liftIO $ pPrint positions

main :: IO ()
main = do
  _ <- C.runCommand app W.empty
  return ()
