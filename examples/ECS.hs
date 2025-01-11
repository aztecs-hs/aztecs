module Main where

import Control.Monad.IO.Class
import Data.Aztecs.Command (Command)
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

app :: Command IO ()
app = do
  e <- C.spawn (Position 0)
  C.insert e (Velocity 0)

  e' <- C.spawn (Position 1)
  C.insert e' (Velocity 1)

  q <-
    Q.all
      ( Q.writeWith
          Q.read
          ( \(Velocity v) (Position p) ->
              (Position (p + v), (p, v))
          )
      )
  liftIO $ pPrint q

main :: IO ()
main = do
  _ <- C.runCommand app W.empty
  return ()
