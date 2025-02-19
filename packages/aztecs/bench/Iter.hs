{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

import Aztecs
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.ECS.World as W
import Criterion.Main

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

run :: World -> IO ()
run w = do
  let s =
        Control.Monad.void
          ( S.map
              ( proc () -> do
                  Velocity v <- Q.fetch -< ()
                  Position p <- Q.fetch -< ()
                  Q.set -< Position $ p + v
              )
          )
  !_ <- S.runSystemWithWorld s w
  return ()

main :: IO ()
main = do
  let !w =
        foldr
          ( \_ wAcc ->
              let (e, wAcc') = W.spawn (bundle $ Position 0) wAcc
               in W.insert e (Velocity 1) wAcc'
          )
          W.empty
          [0 :: Int .. 10000]
  defaultMain [bench "iter" $ nfIO (run w)]
