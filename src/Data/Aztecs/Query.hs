{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query
  ( Query (..),
    fetch,
    all,
    lookup,
  )
where

import Data.Aztecs.Component (Component)
import Data.Aztecs.Core (Entity)
import Data.Aztecs.World (World (..))
import Data.Aztecs.World.Archetypes (Archetype, ArchetypeState (..), archetype)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Aztecs.World.Components (ComponentRef (..), Components)
import qualified Data.Aztecs.World.Components as CS
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Prelude hiding (all, lookup)

data Query a
  = forall s. Query
      (Components -> (s, Archetype, Components))
      (Entity -> s -> ArchetypeState -> Components -> Maybe a)
      (s -> ArchetypeState -> Components -> [(Entity, ComponentRef a)])

instance Functor Query where
  fmap f (Query f' g h) =
    Query
      f'
      (\e s archState cs -> f <$> g e s archState cs)
      (\s archState cs -> map (\(e, cr) -> (e, fmap f cr)) (h s archState cs))

instance Applicative Query where
  pure a = Query (\cs -> ((), mempty, cs)) (\_ _ _ _ -> Just a) (\_ _ _ -> [])
  Query f g h <*> Query f' g' h' =
    Query
      ( \cs ->
          let (s, arch, cs') = f cs
              (s', arch', cs'') = f' cs'
           in ((s, s'), arch <> arch', cs'')
      )
      ( \e (s, s') archState cs -> do
          a <- g e s archState cs
          a' <- g' e s' archState cs
          return $ a a'
      )
      ( \(s, s') archState cs ->
          let xs = h s archState cs
              xs' = h' s' archState cs
              y = Map.fromList xs
              z (e, c) = do
                r <- Map.lookup e y
                return (e, r <*> c)
              xs'' = map z xs'
           in catMaybes xs''
      )

fetch :: forall c. (Component c) => Query c
fetch =
  Query
    ( \cs ->
        let (cId, cs') = CS.insertComponentId @c cs
         in (cId, archetype @c cId, cs')
    )
    ( \e cId (ArchetypeState _ archCs _) cs -> do
        (ComponentRef f) <- A.getArchetypeComponent e cId archCs
        return $ f cs
    )
    ( \cId (ArchetypeState _ archCs _) cs -> A.getArchetypeComponents cId archCs
    )

all :: Query a -> World -> ([a], World)
all (Query f _ h) (World cs as i) =
  let (s, arch, cs') = f cs
      (archId, as') = A.insertArchetype arch cs as
      res = do
        archState <- A.getArchetype archId as'
        return $ h s archState cs'
      res' = map (\(_, (ComponentRef rf)) ->  rf cs') (fromMaybe [] res)
   in (res', World cs' as' i)

lookup :: Entity -> Query a -> World -> (Maybe a, World)
lookup e (Query f g _) (World cs as i) =
  let (s, arch, cs') = f cs
      (archId, as') = A.insertArchetype arch cs as
      res = do
        archState <- A.getArchetype archId as'
        g e s archState cs'
   in (res, World cs' as' i)
