{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.Task as T
import qualified Data.Aztecs.World as W

newtype Position = Position Int

instance Component Position

newtype S = S (Query (Write Position))

instance System IO S where
  access = S <$> query Q.write
  run (S q) = do
    res <- T.all q
    T.alter res (\(Position x) -> Position $ x + 1)

runner :: World -> IO ()
runner w = do
  !_ <- runSystem @IO @S w
  return ()

main :: IO ()
main =
  let !w = foldr (\_ wAcc -> snd (W.spawn (Position 0) wAcc)) W.newWorld [0 :: Int .. 10000]
   in defaultMain [bench "iter" $ nfIO (runner w)]
