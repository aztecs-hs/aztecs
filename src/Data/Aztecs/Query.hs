{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( ReadWrites (..),
    Write,
    unWrite,
    mapWrite,
    Query (..),
    read,
    write,
    query,
    all,
    alter,
  )
where

import qualified Data.Aztecs.Storage as S
import Data.Aztecs.World (Component, Entity, EntityComponent (..), World (..), get, getRow, setRow)
import Data.Aztecs.World.Archetypes (Archetype)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import Prelude hiding (all, read)

-- | Component IDs to read and write.
data ReadWrites = ReadWrites (Set TypeRep) (Set TypeRep)

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites mempty mempty

-- | Query to apply to the `World`.
data Query a
  = Query
      ReadWrites
      Archetype
      ([Entity] -> World -> [(Entity, a)])
      (Entity -> World -> Maybe a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty mempty (\_ _ -> []) (\_ _ -> Just a)
  Query rs arch f g <*> Query rs' arch' f' g' =
    Query
      (rs <> rs')
      (arch <> arch')
      (\es w -> [(e, a b) | (e, a) <- f es w, (_, b) <- f' es w])
      (\e w -> g e w <*> g' e w)

-- | Read a `Component`.
read :: forall c. (Component c) => Query c
read =
  Query
    (ReadWrites (Set.fromList [typeOf (Proxy :: Proxy c)]) Set.empty)
    (A.archetype @c)
    ( \es w ->
        let es' = fromMaybe [] (fmap S.toList (getRow (Proxy :: Proxy c) w))
            es'' = filter (\(EntityComponent e _) -> e `elem` es) es'
         in map (\(EntityComponent e c) -> (e, c)) es''
    )
    get

newtype Write c = Write c deriving (Show)

unWrite :: Write c -> c
unWrite (Write c) = c

mapWrite :: (c -> c) -> Write c -> Write c
mapWrite f (Write c) = Write (f c)

-- | Get a writer to a `Component`.
write :: forall c. (Component c) => Query (Write c)
write =
  Query
    (ReadWrites Set.empty (Set.fromList [typeOf (Proxy :: Proxy c)]))
    (A.archetype @c)
    ( \es w ->
        let es' = fromMaybe [] (fmap S.toList (getRow (Proxy :: Proxy c) w))
            es'' = filter (\(EntityComponent e _) -> e `elem` es) es'
         in map (\(EntityComponent e c) -> (e, Write c)) es''
    )
    (\e w -> Write <$> get e w)

-- | Query a single match from the `World`.
query :: Entity -> Query a -> World -> Maybe a
query e (Query _ _ _ f) w = f e w

-- | Query all matches from the `World`.
all :: Query a -> World -> [(Entity, a)]
all (Query _ arch f _) (World cs as) =
  let (archId, as') = A.insertArchetype arch cs as
      es = A.getArchetype archId as'
   in f es (World cs as)

-- | Alter the components in a query.
alter :: (Component c) => [(Entity, Write c)] -> (c -> c) -> World -> World
alter as g w =
  let s = getRow Proxy w
      s' = fmap (\s'' -> S.insert s'' (map (\(e, Write a) -> EntityComponent e (g a)) as)) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')
