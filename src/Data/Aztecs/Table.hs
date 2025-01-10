module Data.Aztecs.Table
  ( ColumnID (..),
    Column (..),
    TableID (..),
    Table (..),
    singleton,
    lookup,
  )
where

import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (lookup, replicate)

newtype ColumnID = ColumnID {unColumnId :: Int}
  deriving (Eq, Ord, Show)

newtype Column c = Column (Vector c)

newtype TableID = TableID {unTableId :: Int}
  deriving (Eq, Ord, Show)

newtype Table = Table (Vector Dynamic)

singleton :: (Typeable c) => c -> Table
singleton c = Table . V.singleton . toDyn . Column $ V.singleton c

lookup :: (Typeable c) => Table -> TableID -> ColumnID -> Maybe c
lookup (Table table) (TableID tableId) (ColumnID colId) = do
  dyn <- table V.!? tableId
  Column (col) <- fromDynamic dyn
  col V.!? colId
