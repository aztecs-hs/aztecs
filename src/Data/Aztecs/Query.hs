{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Query where

import Data.Aztecs
import Data.Aztecs.Archetypes
import qualified Data.Aztecs.Archetypes as AS
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Table (Column)
import qualified Data.Aztecs.Table as Table
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

newtype Query a
  = Query (World -> (ComponentIDSet, World, ArchetypeID -> Column -> World -> Maybe a))
  deriving (Functor)

instance Applicative Query where
  pure a = Query $ \w -> (ComponentIDSet Set.empty, w, \_ _ _ -> Just a)
  Query f <*> Query a = Query $ \w ->
    let (ComponentIDSet idSetF, w', f'') = f w
        (ComponentIDSet idSetA, w'', a'') = a w'
     in ( ComponentIDSet $ Set.union idSetF idSetA,
          w'',
          \archId table wAcc -> do
            f' <- f'' archId table wAcc
            a' <- a'' archId table wAcc
            return $ f' a'
        )

read :: forall c. (Typeable c) => Query c
read = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
   in ( ComponentIDSet (Set.singleton cId),
        w {components = cs},
        \archId col wAcc -> do
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          Table.lookupColumnId colId col
      )

lookup :: Entity -> Query a -> World -> (Maybe a, World)
lookup e (Query f) w =
  case f w of
    (idSet, w', f') ->
      let res = do
            archId <- Map.lookup idSet (archetypeIds (W.archetypes w'))
            let arch = (AS.archetypes (W.archetypes w')) Map.! archId
            record <- Map.lookup e (entities (W.archetypes w'))
            col <- Table.lookupColumn (recordTableId record) (archetypeTable arch)
            f' archId col w'
       in (res, w')

all :: Query a -> World -> ([a], World)
all (Query f) w =
  case f w of
    (idSet, w', f') -> case Map.lookup idSet (archetypeIds (W.archetypes w')) of
      Just archId ->
        let go i (cAcc, wAcc) =
              let arch = AS.archetypes (W.archetypes w') Map.! i
                  (cs', wAcc') = (fromMaybe [] $ mapM (\col -> f' archId col w') (Table.toList (archetypeTable arch)), wAcc)
                  (cs'', wAcc'') = foldr go ([], wAcc') (Map.elems $ archetypeAdd arch)
               in (cAcc ++ cs' ++ cs'', wAcc'')
            (cs, w'') = go archId ([], w')
         in (cs, w'')
      Nothing -> ([], w')
