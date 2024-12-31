module Data.Aztecs.Storage (Storage (..), table, table') where

import Control.Monad (filterM)
import Data.Aztecs.Core
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)

data Storage a = Storage
  { empty :: IO (Storage a),
    spawn :: Entity -> a -> IO (Storage a),
    get :: Entity -> IO (Maybe (a, a -> Storage a -> IO (Storage a))),
    toList :: IO [EntityComponent a],
    toList' :: IO [(EntityComponent a, a -> IO ())],
    remove :: Entity -> IO (Storage a)
  }

table' :: [IORef (EntityComponent a)] -> Storage a
table' cs =
  Storage
    { empty = pure $ table' [],
      spawn = \e a -> do
        r <- newIORef (EntityComponent e a)
        return $ table' (r : cs),
      get = \e -> do
        cs' <-
          mapM
            ( \r -> do
                a <- readIORef r
                return (a, r)
            )
            cs
        return
          ( find (\(EntityComponent e' _, _) -> e == e') cs'
              <&> \(EntityComponent _ a, r) ->
                ( a,
                  \a' t -> do
                    writeIORef r (EntityComponent e a')
                    return t
                )
          ),
      toList = mapM readIORef cs,
      toList' =
        mapM
          ( \r -> do
              (EntityComponent e a) <- readIORef r
              return
                ( (EntityComponent e a),
                  \a' -> writeIORef r (EntityComponent e a')
                )
          )
          cs,
      remove = \e -> do
        cs' <-
          filterM
            ( \r -> do
                (EntityComponent e' _) <- readIORef r
                return $ e /= e'
            )
            cs
        return $ table' cs'
    }

table :: Storage a
table = table' []
