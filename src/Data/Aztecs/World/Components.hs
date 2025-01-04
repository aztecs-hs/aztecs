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

newtype ComponentId = ComponentId Int deriving (Eq, Ord, Show)

data Components = Components (Map ComponentId Dynamic) Entity (Map TypeRep ComponentId) ComponentId deriving (Show)

newComponents :: Components
newComponents = Components empty (Entity 0) (empty) (ComponentId 0)

union :: Components -> Components -> Components
union (Components a e ids i) (Components b _ _ _) = Components (Map.union a b) e ids i

insertComponentId :: forall c. (Component c) => Components -> (ComponentId,Components)
insertComponentId (Components w e ids (ComponentId i)) =
  (ComponentId i, Components w e (Map.insert (typeOf @(Proxy c) Proxy) (ComponentId i) ids) (ComponentId $ i + 1))

spawn :: forall c. (Component c) => c -> Components -> IO (Entity, Components)
spawn c (Components w (Entity e) ids i) = do
  w' <- insert (Entity e) c (Components w (Entity $ e + 1) ids i)
  return (Entity e, w')

-- | Insert a component into an `Entity`.
-- If the component already exists, it will be replaced.
insert :: forall c. (Component c) => Entity -> c -> Components -> IO Components
insert e c cs = do
  let (cId, (Components w e' ids i)) = insertComponentId @c cs
  w' <-
    Map.alterF
      ( \maybeRow -> do
          s <- S.spawn (fromMaybe storage (maybeRow >>= fromDynamic)) e c
          return . Just $ toDyn s
      )
      cId
      w
  return $ Components w' e' ids i

adjust :: (Component c) => c -> (c -> c) -> Entity -> Components -> IO Components
adjust a f w = insert w (f a)

getRow :: (Component c) => Proxy c -> Components -> Maybe (Storage c)
getRow p (Components w _ ids _) = do
  cId <- Map.lookup (typeOf p) ids
  Data.Map.lookup cId w >>= fromDynamic

get :: forall c. (Component c) => Entity -> Components -> IO (Maybe (c, c -> Components -> IO Components))
get e (Components w _ ids _) =
  let maybeS = do
        cId <- Data.Map.lookup (typeOf @(Proxy c) Proxy) ids
        d <- Data.Map.lookup cId w
        fromDynamic d
   in case maybeS of
        Just s -> do
          res <- S.get s e
          case res of
            Just (c, f) ->
              return $
                Just
                  ( c,
                    \c' (Components w' e' ids' i) ->
                      let cId = fromMaybe (error "TODO") (Map.lookup (typeOf @(Proxy c) Proxy) ids')
                       in return $
                            Components
                              (alter (\row -> Just . toDyn $ f c' (fromMaybe storage (row >>= fromDynamic))) cId w')
                              e'
                              ids
                              i
                  )
            Nothing -> return Nothing
        Nothing -> return Nothing

remove :: forall c. (Component c) => Entity -> Components -> Components
remove e (Components w e' ids i) =
  let cId = fromMaybe i (Map.lookup (typeOf @(Proxy c) Proxy) ids)
   in Components (alter (\row -> row >>= f) cId w) e' ids i
  where
    f row = fmap (\row' -> toDyn $ S.remove @c row' e) (fromDynamic row)
