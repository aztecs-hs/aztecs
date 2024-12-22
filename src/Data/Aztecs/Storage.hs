module Data.Aztecs.Storage (Storage (..), table, table') where

import Data.Aztecs.Core
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (isJust)

data Storage a = Storage
  { empty :: Storage a,
    spawn :: Entity -> a -> Storage a,
    insert :: [EntityComponent a] -> Storage a,
    get :: Entity -> Maybe a,
    toList :: [EntityComponent a],
    remove :: Entity -> Storage a
  }

table' :: [EntityComponent a] -> Storage a
table' cs =
  Storage
    { empty = table' [],
      spawn = \e a -> table' (EntityComponent e a : cs),
      insert = \cs' -> table' (filter (\(EntityComponent e _) -> not . isJust $ find (\(EntityComponent e' _) -> e == e') cs') cs <> cs'),
      get = \e ->
        find (\(EntityComponent e' _) -> e == e') cs
          <&> \(EntityComponent _ a) -> a,
      toList = cs,
      remove = \e -> table' (filter (\(EntityComponent e' _) -> e /= e') cs)
    }

table :: Storage a
table = table' []
