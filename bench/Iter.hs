{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Aztecs
import Data.Aztecs.Query (mapWrite)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.System (runSystem)
import qualified Data.Aztecs.Task as T
import qualified Data.Aztecs.World as W

newtype Position = Position Int

instance Component Position

newtype Velocity = Velocity Int

instance Component Velocity

newtype S = S (Query ((Write Position), Velocity))

instance System IO S where
  access = S <$> query ((,) <$> Q.write <*> Q.read)
  run (S q) = do
    res <- T.all q
    let res' =
          fmap
            ( \(p, Velocity v) ->
                mapWrite
                  (\(Position p') -> Position $ v + p')
                  p
            )
            res
    T.alter res' id

runner :: World -> IO ()
runner w = do
  !_ <- runSystem @IO @S w
  return ()

main :: IO ()
main =
  let !w = foldr (\_ wAcc -> snd (W.spawn (Position 0) wAcc)) W.newWorld [0 :: Int .. 10000]
   in defaultMain [bench "iter" $ nfIO (runner w)]
