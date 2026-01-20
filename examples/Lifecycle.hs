{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import Control.Monad.IO.Class

newtype X = X Int deriving (Show)

instance Component IO X where
  componentOnInsert = exampleHook "insert"
  componentOnChange = exampleHook "change"
  componentOnRemove = exampleHook "remove"

exampleHook :: String -> EntityID -> X -> Access IO ()
exampleHook s e x = liftIO . putStrLn $ s ++ " " ++ show e ++ ": " ++ show x

addTen :: Query IO X
addTen = queryMap (\(X n) -> X (n + 10))

app :: Access IO ()
app = do
  e <- spawn $ bundle (X 1)
  _ <- system $ runQuery addTen
  _ <- remove @IO @X e
  return ()

main :: IO ()
main = runAccess_ app
