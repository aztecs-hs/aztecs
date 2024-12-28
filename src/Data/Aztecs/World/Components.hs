{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.World.Components
  ( Component (..),
    Components,
    union,
    spawn,
    insert,
    adjust,
    get,
    getRow,
    newComponents,
    setRow,
    remove,
  )
where

import Data.Aztecs.Core
import Data.Aztecs.Storage (Storage, table)
import qualified Data.Aztecs.Storage as S
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Map (Map, alter, empty, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable
import Prelude hiding (read)

class (Typeable a) => Component a where
  storage :: Storage a
  storage = table

data Components = Components (Map TypeRep Dynamic) Entity deriving (Show)

newComponents :: Components
newComponents = Components empty (Entity 0)

union :: Components -> Components -> Components
union (Components a e) (Components b _) = Components (Map.union a b) e

spawn :: forall c. (Component c) => c -> Components -> IO (Entity, Components)
spawn c (Components w (Entity e)) = do
  w' <- insert (Entity e) c (Components w (Entity $ e + 1))
  return (Entity e, w')

insert :: forall c. (Component c) => Entity -> c -> Components -> IO Components
insert e c (Components w e') = do
  w' <-
    Map.alterF
      ( \maybeRow -> do
          s <- S.spawn (fromMaybe storage (maybeRow >>= fromDynamic)) e c
          return . Just $ toDyn s
      )
      (typeOf (Proxy :: Proxy c))
      w
  return $ Components w' e'

adjust :: (Component c) => c -> (c -> c) -> Entity -> Components -> IO Components
adjust a f w = insert w (f a)

getRow :: (Component c) => Proxy c -> Components -> Maybe (Storage c)
getRow p (Components w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: forall c. (Component c) => Entity -> Components -> IO (Maybe (c, c -> Components -> IO Components))
get e (Components w _) = case Data.Map.lookup (typeOf @(Proxy c) Proxy) w >>= fromDynamic of
  Just s -> do
    res <- S.get s e
    case res of
      Just (c, f) ->
        return $
          Just
            ( c,
              \c' (Components w' e') ->
                return $ Components (alter (\row -> Just . toDyn $ f c' (fromMaybe storage (row >>= fromDynamic))) (typeOf @(Proxy c) Proxy) w') e'
            )
      Nothing -> return Nothing
  Nothing -> return Nothing

setRow :: forall c. (Component c) => Storage c -> Components -> Components
setRow cs (Components w e') = Components (Map.insert (typeOf @(Proxy c) Proxy) (toDyn cs) w) e'

remove :: forall c. (Component c) => Entity -> Components -> Components
remove e (Components w e') = Components (alter (\row -> row >>= f) (typeOf @(Proxy c) Proxy) w) e'
  where
    f row = fmap (\row' -> toDyn $ S.remove @c row' e) (fromDynamic row)
