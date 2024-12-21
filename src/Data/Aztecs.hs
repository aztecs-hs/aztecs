{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Aztecs
  ( Entity,
    EntityComponent (..),
    Storage (..),
    table,
    Component (..),
    World,
    newWorld,
    spawn,
    insert,
    get,
    Query,
    read,
    Write (..),
    update,
    updateQuery,
    updateQueryTask,
    write,
    query,
    QueryResult (..),
    queryAll,
    Access (..),
    queryAccess,
    Task (..),
    spawnTask,
    insertTask,
    queryTask,
    System (..),
    runSystem,
  )
where

import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import qualified Control.Monad.State as S
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (Map, alter, empty, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable
import Prelude hiding (read)

newtype Entity = Entity Int deriving (Eq, Show)

data EntityComponent a = EntityComponent Entity a deriving (Show)

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty' = table' [],
      spawn' = \e a -> table' (EntityComponent e a : cs),
      insert' = \cs' -> table' (filter (\(EntityComponent e _) -> not . isJust $ find (\(EntityComponent e' _) -> e == e') cs') cs <> cs'),
      get' = \e ->
        find (\(EntityComponent e' _) -> e == e') cs
          <&> \(EntityComponent _ a) -> a,
      toList = cs
    }

table :: Storage a
table = table' []

data Storage a = Storage
  { empty' :: Storage a,
    spawn' :: Entity -> a -> Storage a,
    insert' :: [EntityComponent a] -> Storage a,
    get' :: Entity -> Maybe a,
    toList :: [EntityComponent a]
  }

class Component a where
  storage :: Storage a
  storage = table

data World = World (Map TypeRep Dynamic) Entity deriving (Show)

newWorld :: World
newWorld = World empty (Entity 0)

spawn :: (Component c, Typeable c) => c -> World -> (Entity, World)
spawn = f (Proxy)
  where
    f :: (Component c, Typeable c) => Proxy c -> c -> World -> (Entity, World)
    f p c (World w (Entity e)) =
      ( Entity e,
        World
          ( alter
              ( \maybeRow -> case maybeRow of
                  Just row -> fmap (\row' -> toDyn $ spawn' row' (Entity e) c) (fromDynamic row)
                  Nothing -> Just $ toDyn $ spawn' storage (Entity e) c
              )
              (typeOf p)
              w
          )
          (Entity (e + 1))
      )

insert :: (Component c, Typeable c) => Entity -> c -> World -> World
insert = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Entity -> c -> World -> World
    f p e c (World w e') =
      World
        ( alter
            ( \maybeRow -> Just $ toDyn $ g (maybeRow >>= fromDynamic) e c
            )
            (typeOf p)
            w
        )
        e'
    g :: (Component c, Typeable c) => Maybe (Storage c) -> Entity -> c -> Storage c
    g maybeRow e c = case maybeRow of
      Just row -> spawn' row e c
      Nothing -> spawn' storage e c

getRow :: (Typeable c) => Proxy c -> World -> Maybe (Storage c)
getRow p (World w _) = Data.Map.lookup (typeOf p) w >>= fromDynamic

get :: (Typeable c) => Entity -> World -> Maybe c
get e (World w _) = f (Proxy)
  where
    f :: (Typeable c) => Proxy c -> Maybe c
    f p = Data.Map.lookup (typeOf p) w >>= fromDynamic >>= flip get' e

setRow :: (Component c, Typeable c) => Storage c -> World -> World
setRow = f Proxy
  where
    f :: (Component c, Typeable c) => Proxy c -> Storage c -> World -> World
    f p cs (World w e') =
      World
        ( Map.insert (typeOf p) (toDyn cs) w
        )
        e'

data ReadWrites = ReadWrites [TypeRep] [TypeRep]

instance Semigroup ReadWrites where
  ReadWrites rs ws <> ReadWrites rs' ws' = ReadWrites (rs <> rs') (ws <> ws')

instance Monoid ReadWrites where
  mempty = ReadWrites [] []

data Query a
  = Query
      ReadWrites
      (Maybe [Entity] -> World -> ([Entity], [a]))
      (Entity -> World -> Maybe a)
  deriving (Functor)

instance Applicative Query where
  pure a = Query mempty (\_ _ -> ([], [a])) (\_ _ -> Just a)
  Query rs f g <*> Query rs' f' g' =
    Query
      (rs <> rs')
      ( \es w ->
          let (es1, fs) = f es w
           in case es1 of
                [] -> ([], [])
                _ ->
                  let (es2, as) = f' es w
                   in (es1 <> es2, fs <*> as)
      )
      ( \e w ->
          case g e w of
            Just a -> case g' e w of
              Just a' -> Just $ a a'
              Nothing -> Nothing
            Nothing -> Nothing
      )

read :: (Typeable a) => Query a
read = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query a
    f p = Query (ReadWrites [typeOf p] []) (\es w -> readWrite es p w) (get)

newtype Write a = Write {unWrite :: a} deriving (Show)

update :: (Component c, Typeable c) => Write c -> (c -> c) -> Entity -> World -> World
update (Write a) f w = insert w (f a)

write :: (Component a, Typeable a) => Query (Write a)
write = f Proxy
  where
    f :: (Typeable a) => Proxy a -> Query (Write a)
    f p =
      Query
        (ReadWrites [] [typeOf p])
        (\es w -> let (a, b) = readWrite es p w in (a, Write <$> b))
        (\e w -> Write <$> get e w)

readWrite :: (Typeable a, Foldable t) => Maybe (t Entity) -> Proxy a -> World -> ([Entity], [a])
readWrite es p w =
  let row = (fromMaybe [] (fmap toList (getRow p w)))
      row' = case es of
        Just es' -> (filter (\(EntityComponent e _) -> isJust $ find (== e) es') row)
        Nothing -> row
   in foldr (\(EntityComponent e a) (es'', as) -> (e : es'', a : as)) ([], []) row'

query :: Entity -> Query a -> World -> Maybe a
query e (Query _ _ f) w = f e w

data QueryResult a = QueryResult [Entity] [a]
  deriving (Functor, Show)

queryAll :: Query a -> World -> QueryResult a
queryAll (Query _ f _) w = let (es, as) = f Nothing w in QueryResult es as

updateQuery :: (Component a, Typeable a) => QueryResult (Write a) -> (a -> a) -> World -> World
updateQuery (QueryResult es as) g w =
  let as' = map (\(Write wr) -> g wr) as
      s = getRow Proxy w
      s' = fmap (\s'' -> insert' s'' (map (\(e, a) -> EntityComponent e a) (zip es as'))) s
   in fromMaybe w (fmap (\y' -> setRow y' w) s')

newtype Access m a = Access {unAccess :: World -> m (ReadWrites, a)}
  deriving (Functor)

instance (Applicative m) => Applicative (Access m) where
  pure a = Access $ (\_ -> pure (mempty, a))
  Access f <*> Access a = Access $ (\w -> (\(rs, f') (rs', a') -> (rs <> rs', f' a')) <$> f w <*> a w)

queryAccess :: (Applicative m) => Query a -> Access m (Query a)
queryAccess (Query a f g) = Access $ (\_ -> pure (a, (Query a f g)))

data Task m s a = Task (StateT (s, World) m a)
  deriving (Functor)

instance (Monad m) => Applicative (Task m s) where
  pure a = Task $ pure a
  Task f <*> Task a = Task $ f <*> a

instance (Monad m) => Monad (Task m s) where
  Task a >>= f = Task $ a >>= (\a' -> case f a' of Task b -> b)

instance (MonadIO m) => MonadIO (Task m s) where
  liftIO a = Task $ liftIO a

spawnTask :: (Component a, Typeable a, Monad m) => a -> Task m s Entity
spawnTask a = Task $ do
  (s, w) <- S.get
  let (e, w') = spawn a w
  S.put $ (s, w')
  return e

insertTask :: (Component a, Typeable a, Monad m) => Entity -> a -> Task m s ()
insertTask e a = Task $ do
  (s, w) <- S.get
  S.put $ (s, insert e a w)
  return ()

queryTask :: (Monad m) => Query a -> Task m s (QueryResult a)
queryTask q = Task $ do
  (_, w) <- S.get
  return $ queryAll q w

updateQueryTask :: (Component a, Typeable a, Monad m) => QueryResult (Write a) -> (a -> a) -> Task m s ()
updateQueryTask q f = Task $ do
  (s, w) <- S.get
  S.put $ (s, updateQuery q f w)
  return ()

class System m a where
  access :: Access m a
  run :: a -> Task m a ()

runSystem :: (Monad m, System m a) => World -> m (a, World)
runSystem w = do
  (_, i :: a) <- unAccess access w
  let (Task t) = run i
  runStateT t (i, w) <&> snd
