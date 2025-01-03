{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.System (Cache, runSystem')
import qualified Data.Aztecs.System as S
import qualified Data.Aztecs.World as W
import Data.Foldable (foldrM)

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data S

instance System IO S where
  access = do
    _ <- S.all $ do
      Velocity y <- Q.read
      Q.write (\(Position x) -> Position (x + y))
    return ()

runner :: Cache -> World -> IO ()
runner c w = do
  !_ <- runSystem' @S c w
  return ()

main :: IO ()
main = do
  !w <-
    foldrM
      ( \_ wAcc -> do
          (e, wAcc') <- W.spawn (Position 0) wAcc
          wAcc'' <- W.insert e (Velocity 1) wAcc'
          return wAcc''
      )
      W.newWorld
      [0 :: Int .. 10000]
  !(c, w') <- runSystem' @S mempty w
  defaultMain [bench "iter" $ nfIO (runner c w')]
