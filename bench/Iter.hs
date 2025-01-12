{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Aztecs.Edit (Edit)
import qualified Data.Aztecs.Edit as E
import Data.Aztecs.Entity (Component, entity, (:&) (..), (<&>))
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

app :: Edit IO ()
app = do
  !_ <- Q.map $
    \(Position p :& Velocity v) -> Position (p + v)

  return ()

run :: W.World -> IO ()
run w = do
  !_ <- E.runEdit app w
  return ()

main :: IO ()
main = do
  let w =
        foldr
          ( \_ wAcc -> snd $ W.spawn (entity (Position 0) <&> Velocity 1) wAcc
          )
          W.empty
          [0 :: Int .. 10000]
  defaultMain [bench "iter" $ nfIO (run w)]
