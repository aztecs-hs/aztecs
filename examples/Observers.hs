{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import Control.Monad.IO.Class

newtype Health = Health Int
  deriving (Show)

instance Component IO Health

heal :: Query IO Health
heal = queryMap (\(Health h) -> Health (h + 10))

run :: Access IO ()
run = do
  -- Spawn a player with health
  player <- spawn . bundle $ Health 100

  -- Spawn observers that react to lifecycle events on the player's Health component
  _ <- spawn . bundle . observer @IO @(OnInsert Health) player $ \e (OnInsert h) ->
    observe "insert" e h
  _ <- spawn . bundle . observer @IO @(OnChange Health) player $ \e change ->
    liftIO . putStrLn $ "change " ++ show e ++ ": old=" ++ show (onChangeOld change) ++ ", new=" ++ show (onChangeNew change)
  _ <- spawn . bundle . observer @IO @(OnRemove Health) player $ \e (OnRemove h) ->
    observe "remove" e h

  -- Trigger events by inserting, changing, and removing the Health component
  _ <- insert player . bundle $ Health 150
  _ <- system $ runQuery heal
  _ <- remove @_ @Health player

  return ()
  where
    observe s e h = liftIO . putStrLn $ s ++ " " ++ show e ++ ": " ++ show h

main :: IO ()
main = runAccess_ run
