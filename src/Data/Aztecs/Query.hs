{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query where

import Data.Aztecs (Archetype (Archetype), ArchetypeID, ComponentIDSet (ComponentIDSet), ComponentState (componentColumnIds), Entity, EntityRecord (recordTableId), World (..), insertId)
import Data.Aztecs.Table (Table)
import qualified Data.Aztecs.Table as Table
import Data.Data (Typeable)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Query a
  = forall s. Query
      ( World ->
        ( ComponentIDSet,
          s,
          World,
          ArchetypeID ->
          EntityRecord ->
          Table ->
          s ->
          World ->
          Maybe a
        )
      )

instance Functor Query where
  fmap f (Query q) = Query $ \w ->
    let (idSet, s, w', f') = q w
     in (idSet, s, w', \archId record table s' w'' -> f <$> f' archId record table s' w'')

instance Applicative Query where
  pure a = Query $ \w -> (ComponentIDSet Set.empty, a, w, \_ _ _ a' _ -> Just a')
  Query f <*> Query a = Query $ \w ->
    let (ComponentIDSet idSetF, s, w', f'') = f w
        (ComponentIDSet idSetA, s', w'', a'') = a w'
     in ( ComponentIDSet $ Set.union idSetF idSetA,
          (s, s'),
          w'',
          \archId record table (sAcc, sAcc') wAcc -> do
            f' <- f'' archId record table sAcc wAcc
            a' <- a'' archId record table sAcc' wAcc
            return $ f' a'
        )

read :: forall c. (Typeable c) => Query c
read = Query $ \w ->
  let (cId, w') = insertId @c w
   in ( ComponentIDSet (Set.singleton cId),
        cId,
        w',
        \archId record table cId' wAcc -> do
          cState <- Map.lookup cId' (componentStates wAcc)
          colId <- Map.lookup archId (componentColumnIds cState)
          Table.lookup table (recordTableId record) colId
      )

lookup :: Entity -> Query a -> World -> (Maybe a, World)
lookup e (Query f) w =
  case f w of
    (idSet, s, w', f') ->
      let res = do
            archId <- Map.lookup idSet (archetypeIds w')
            let Archetype _ table = (archetypes w') Map.! archId
            record <- Map.lookup e (entities w')
            f' archId record table s w'
       in (res, w')
