module Data.Aztecs.Storage
  ( Storage (..),
    Table (..),
    table,
    table',
  )
where

import Data.Aztecs.Core (Entity)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup)

data Storage a = Storage
  { insert :: Entity -> a -> Storage a,
    lookup :: Entity -> Maybe a
  }

newtype Table c = Table (Vector (Entity, c))

table :: Storage c
table = table' (Table (V.empty))

table' :: Table c -> Storage c
table' (Table t) =
  Storage
    { insert = \e c -> table' $ Table (V.concat [V.singleton (e, c), t]),
      lookup = \e -> snd <$> V.find (\(e', _) -> e' == e) t
    }
