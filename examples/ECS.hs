{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Arrow ((>>>))
import Control.DeepSeq
import GHC.Generics (Generic)

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

setup :: (ArrowQueueSystem b m arr) => arr () ()
setup = S.queue . const . A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)

move :: (ArrowQuery q, ArrowSystem q arr) => arr () [Position]
move =
  S.map
    ( proc () -> do
        Velocity v <- Q.fetch -< ()
        Position p <- Q.fetch -< ()
        Q.set -< Position $ p + v
    )

app :: Schedule IO () ()
app = system setup >>> forever (system move) print

main :: IO ()
main = runSchedule_ app
