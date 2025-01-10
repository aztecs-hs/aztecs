module Data.Aztecs.Table
  ( ColumnID (..),
    Column (..),
    TableID (..),
    Table (..),
    empty,
    replicate,
    lookup,
  )
where

import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVector
import Prelude hiding (lookup, replicate)

newtype ColumnID = ColumnID {unColumnId :: Int}
  deriving (Eq, Ord, Show)

newtype Column c = Column (IOVector c)

newtype TableID = TableID {unTableId :: Int}
  deriving (Eq, Ord, Show)

newtype Table = Table (IOVector Dynamic)

empty :: IO Table
empty = Table <$> MVector.new 0

replicate :: (Typeable c) => Int -> c -> IO Table
replicate n c =
  Table
    <$> MVector.replicateM
      1
      ( do
          col <- MVector.replicate n c
          return . toDyn $ Column col
      )

lookup :: (Typeable c) => Table -> TableID -> ColumnID -> IO (Maybe c)
lookup (Table table) (TableID tableId) (ColumnID colId) = do
  dyn <- MVector.read table tableId
  case fromDynamic dyn of
    Just (Column col) -> do
      c <- MVector.read col colId
      return . Just $ c
    Nothing -> return Nothing
