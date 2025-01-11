{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.Query
  ( Query (..),
    read,
    write,
    writeWith,
    all,
    all',
    lookup,
    lookup',
  )
where

import Control.Monad.State (MonadState (..))
import Data.Aztecs
import Data.Aztecs.Archetypes
import qualified Data.Aztecs.Archetypes as AS
import Data.Aztecs.Command (Command (..))
import qualified Data.Aztecs.Components as CS
import Data.Aztecs.Entity (Entity (..))
import Data.Aztecs.Table (Column)
import qualified Data.Aztecs.Table as Table
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Data (Typeable)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Prelude hiding (all, lookup, read)

newtype Query a
  = Query (World -> (ComponentIDSet, World, ArchetypeID -> Column -> World -> Maybe (a, Column)))
  deriving (Functor)

instance Applicative Query where
  pure a = Query $ \w -> (ComponentIDSet Set.empty, w, \_ _ _ -> Just (a, mempty))
  Query f <*> Query a = Query $ \w ->
    let (ComponentIDSet idSetF, w', f'') = f w
        (ComponentIDSet idSetA, w'', a'') = a w'
     in ( ComponentIDSet $ Set.union idSetF idSetA,
          w'',
          \archId col wAcc -> do
            (f', col') <- f'' archId col wAcc
            (a', col'') <- a'' archId col' wAcc
            return (f' a', col'')
        )

class Queryable a where
  query :: Query a

instance Queryable (Entity '[]) where
  query = pure ENil

instance (Typeable t, Queryable (Entity ts)) => Queryable (Entity (t ': ts)) where
  query = ECons <$> read @t <*> query @(Entity ts)

read :: forall c. (Typeable c) => Query c
read = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
   in ( ComponentIDSet (Set.singleton cId),
        w {components = cs},
        \archId col wAcc -> do
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col
          return (c, col)
      )

write :: forall c. (Typeable c) => (c -> c) -> Query c
write f = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
   in ( ComponentIDSet (Set.singleton cId),
        w {components = cs},
        \archId col wAcc -> do
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col
          let c' = f c
          return (c', Table.colInsert colId c' col)
      )

writeWith :: forall a b c. (Typeable c) => Query a -> (a -> c -> (c, b)) -> Query b
writeWith (Query q) f = Query $ \w ->
  let (cId, cs) = CS.insert @c (components w)
      (cIds, w', g) = q w
   in ( ComponentIDSet (Set.insert cId (unComponentIdSet cIds)),
        w' {components = cs},
        \archId col wAcc -> do
          (a, col') <- g archId col wAcc
          cState <- Map.lookup cId (componentStates (W.archetypes wAcc))
          colId <- Map.lookup archId (componentColumnIds cState)
          c <- Table.lookupColumnId colId col'
          let (c', b) = f a c
          return (b, Table.colInsert colId c' col')
      )

all :: forall q m. (Queryable (Entity q), Monad m) => Command m [Entity q]
all = queryAll (query @(Entity q))

queryAll :: (Monad m) => Query a -> Command m [a]
queryAll q = Command $ do
  w <- get
  let (as, w') = all' q w
  put w'
  return as

all' :: Query a -> World -> ([a], World)
all' (Query f) w =
  case f w of
    (idSet, w', f') -> case Map.lookup idSet (archetypeIds (W.archetypes w')) of
      Just archId ->
        let go i (cAcc, wAcc) =
              let arch = AS.archetypes (W.archetypes w') Map.! i
                  ((cs', cols), wAcc') = (unzip $ fromMaybe [] $ mapM (\col -> f' archId col w') (Table.toList (archetypeTable arch)), wAcc)
                  (cs'', wAcc'') = foldr go ([], wAcc') (Map.elems $ archetypeAdd arch)
                  archs = (W.archetypes wAcc'')
                  wAcc''' =
                    wAcc''
                      { W.archetypes =
                          archs
                            { AS.archetypes =
                                Map.insert
                                  archId
                                  (arch {archetypeTable = Table.fromList cols})
                                  (AS.archetypes $ archs)
                            }
                      }
               in (cAcc ++ cs' ++ cs'', wAcc''')
            (cs, w'') = go archId ([], w')
         in (cs, w'')
      Nothing -> ([], w')

lookup' :: EntityID -> Query a -> World -> Maybe (a, World)
lookup' e (Query f) w =
  case f w of
    (idSet, w', f') -> do
      archId <- Map.lookup idSet (archetypeIds (W.archetypes w'))
      let arch = (AS.archetypes (W.archetypes w')) Map.! archId
      record <- Map.lookup e (entities (W.archetypes w'))
      col <- Table.lookupColumn (recordTableId record) (archetypeTable arch)
      (a, col') <- f' archId col w'
      let archs = W.archetypes w'
      return
        ( a,
          w'
            { W.archetypes =
                archs
                  { AS.archetypes =
                      Map.insert
                        archId
                        ( arch
                            { archetypeTable =
                                Table.insertCol (recordTableId record) col' (archetypeTable arch)
                            }
                        )
                        (AS.archetypes $ archs)
                  }
            }
        )

lookup :: (Monad m) => EntityID -> Query a -> Command m (Maybe a)
lookup e q = Command $ do
  w <- get
  case lookup' e q w of
    Just (a, w') -> do
      put w'
      return $ Just a
    Nothing -> return Nothing
