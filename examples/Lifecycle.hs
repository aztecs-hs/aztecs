{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import Control.Monad.IO.Class

newtype X = X Int deriving (Show)

instance Component IO X where
  componentOnInsert x = liftIO . putStrLn $ "insert: " ++ show x
  componentOnChange x = liftIO . putStrLn $ "change: " ++ show x
  componentOnRemove x = liftIO . putStrLn $ "remove: " ++ show x

addTen :: QueryT IO X
addTen = fetchMap (\_ (X n) -> X (n + 10)) (pure ())

app :: AccessT IO ()
app = do
  e <- spawn $ bundle (X 1)
  _ <- systemM $ queryM addTen
  _ <- remove @IO @X e
  return ()

main :: IO ()
main = runAccessT_ app
