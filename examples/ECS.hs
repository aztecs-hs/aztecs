module Main where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Monad.IO.Class
import Data.Function ((&))

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

move :: QueryT IO Position
move = Q.fetch & Q.adjust (\(Velocity v) (Position p) -> Position $ p + v)

run :: AccessT IO ()
run = do
  positions <- S.map move
  liftIO $ print positions

app :: AccessT IO ()
app = do
  A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
  run

main :: IO ()
main = runAccessT_ app
