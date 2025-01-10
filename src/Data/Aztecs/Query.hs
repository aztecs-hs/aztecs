{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query where

import Data.Aztecs
import qualified Data.Aztecs as W
import Data.Aztecs.Archetypes
import qualified Data.Aztecs.Archetypes as AS
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Table (Column)
import qualified Data.Aztecs.Table as Table
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

data Query a
  = forall s. Query
      ( World ->
        ( ComponentIDSet,
          s,
          World,
          ArchetypeID -> Column -> s -> World -> Maybe a
        )
      )

instance Functor Query where
  fmap f (Query q) = Query $ \w ->
    let (idSet, s, w', f') = q w
     in (idSet, s, w', \archId table s' w'' -> f <$> f' archId table s' w'')

instance Applicative Query where
  pure a = Query $ \w -> (ComponentIDSet Set.empty, a, w, \_ _ a' _ -> Just a')
  Query f <*> Query a = Query $ \w ->
    let (ComponentIDSet idSetF, s, w', f'') = f w
        (ComponentIDSet idSetA, s', w'', a'') = a w'
     in ( ComponentIDSet $ Set.union idSetF idSetA,
          (s, s'),
          w'',
          \archId table (sAcc, sAcc') wAcc -> do
            f' <- f'' archId table sAcc wAcc
            a' <- a'' archId table sAcc' wAcc
            return $ f' a'
        )

read :: forall c. (Typeable c) => Query c
read = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
   in ( ComponentIDSet (Set.singleton cId),
        cId,
        w {components = cs},
        \archId col cId' wAcc -> do
          cState <- Map.lookup cId' (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          Table.lookupColumnId colId col
      )

lookup :: Entity -> Query a -> World -> (Maybe a, World)
lookup e (Query f) w =
  case f w of
    (idSet, s, w', f') ->
      let res = do
            archId <- Map.lookup idSet (archetypeIds (W.archetypes w'))
            let Archetype _ table = (AS.archetypes (W.archetypes w')) Map.! archId
            record <- Map.lookup e (entities (W.archetypes w'))
            col <- Table.lookupColumn (recordTableId record) table
            f' archId col s w'
       in (res, w')

all :: Query a -> World -> ([a], World)
all (Query f) w =
  case f w of
    (idSet, s, w', f') -> case Map.lookup idSet (archetypeIds (W.archetypes w')) of
      Just archId ->
        let Archetype _ table = (AS.archetypes (W.archetypes w')) Map.! archId
         in (fromMaybe [] $ mapM (\col -> f' archId col s w') (Table.toList table), w')
      Nothing -> ([], w')
