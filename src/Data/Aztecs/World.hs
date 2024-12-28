{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World
  ( Entity,
    EntityComponent (..),
    Component (..),
    World (..),
    union,
    spawn,
    insert,
    get,
    getRow,
    newWorld,
    setRow,
    remove,
  )
where

import Data.Aztecs.Core
import Data.Aztecs.Storage (Storage)
import Data.Aztecs.World.Archetypes (Archetypes, newArchetypes)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Aztecs.World.Components (Component, Components, newComponents)
import qualified Data.Aztecs.World.Components as C
import Data.Typeable
import Prelude hiding (read)

data World = World Components Archetypes deriving (Show)

newWorld :: World
newWorld = World newComponents newArchetypes

union :: World -> World -> World
union (World cs as) (World cs' _) = World (C.union cs cs') as

spawn :: forall c. (Component c) => c -> World -> IO (Entity, World)
spawn c (World cs as) = do
  (e, cs') <- C.spawn c cs 
  return (e, World cs' as)

insert :: forall c. (Component c) => Entity -> c -> World -> IO World
insert e c (World cs as) = do
  cs' <- C.insert e c cs
  return $ World cs' (A.insert @c e cs' as)

getRow :: (Component c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World cs _) = C.getRow p cs

get :: forall c. (Component c) => Entity -> World -> IO (Maybe (c, c -> World -> IO World))
get e (World cs _) = do
  res <- C.get e cs
  case res of
    Just (c, f) ->
      return $
        Just
          ( c,
            \c' (World cs' as) -> do
              cs'' <- f c' cs'
              return $ World cs'' as
          )
    Nothing -> return Nothing

setRow :: forall c. (Component c) => Storage c -> World -> World
setRow row (World cs as) = World (C.setRow row cs) as

remove :: forall c. (Component c) => Entity -> World -> World
remove e (World cs as) = World (C.remove @c e cs) as
