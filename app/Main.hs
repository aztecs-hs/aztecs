{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs as ECS

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data Q = Q (Write X) Y deriving (Show)

data S = S (Query Q)

instance (MonadIO m) => System m S where
  access = S <$> queryAccess (Q <$> ECS.write <*> ECS.read)
  run (S q) = do
    e <- spawnTask (X 0)
    insertTask e (Y 1)

    q' <- queryTask q
    liftIO $ print q'

    updateQueryTask (fmap (\(Q x _) -> x) q') (\(X x) -> X $ x + 1)

    q'' <- queryTask q
    liftIO $ print q''

    return ()

main :: IO ()
main = do
  _ <- runSystem @_ @S newWorld
  return ()
