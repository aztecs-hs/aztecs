{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import Control.Monad.IO.Class

newtype X = X Int deriving (Show)

instance Component IO X where
  componentOnInsert e x = liftIO . putStrLn $ "insert " ++ show e ++ ": " ++ show x
  componentOnChange e x = liftIO . putStrLn $ "change " ++ show e ++ ": " ++ show x
  componentOnRemove e x = liftIO . putStrLn $ "remove " ++ show e ++ ": " ++ show x

addTen :: Query IO X
addTen = fetchMap (\(X n) -> X (n + 10))

app :: Access IO ()
app = do
  e <- spawn $ bundle (X 1)
  _ <- systemM $ query addTen
  _ <- remove @IO @X e
  return ()

main :: IO ()
main = runAccess_ app
