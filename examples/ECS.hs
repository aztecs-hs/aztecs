{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

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
  -- Query for and update all matching entities
  q <- Q.map (\(Velocity v :& Position x) -> Position (x + v))
  liftIO $ print q

main :: IO ()
main = do
  let w =
        foldr
          ( \i wAcc ->
              let (e, wAcc') = W.spawn (Position i) wAcc
               in W.insert e (Velocity 1) wAcc'
          )
          W.empty
          [0 :: Int .. 2]
  _ <- runAccess app w
  print w
