{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Table
  ( ColumnID (..),
    Column (..),
    TableID (..),
    Table (..),
    singleton,
    lookup,
    cons,
    insert,
    remove,
  )
where

import Control.Monad.ST
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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

remove :: (Typeable c) => TableID -> ColumnID -> Table -> Maybe (c, Table)
remove (TableID tableId) (ColumnID colId) (Table table) = do
  dyn <- table V.!? tableId
  Column col <- fromDynamic dyn
  let (left, right) = V.splitAt colId col
      c = V.head right
      col' = left V.++ V.tail right
   in Just (c, Table (table V.// [(tableId, toDyn $ Column col')]))

insert :: (Typeable c) => TableID -> ColumnID -> c -> Table -> Table
insert (TableID tableId) (ColumnID colId) c (Table table) =
  let h v = MV.write v colId c
      g d = case fromDynamic d of
        Just (Column col) -> toDyn $ V.modify h col
        Nothing -> error "TODO"
      f :: MV.MVector s Dynamic -> ST s ()
      f v = MV.modify v g tableId
   in Table $ V.modify f table

cons :: (Typeable c) => TableID -> c -> Table -> (ColumnID, Table)
cons (TableID tableId) c (Table table) =
  let g d = case fromDynamic d of
        Just (Column col) -> toDyn $ V.cons c col
        Nothing -> error "TODO"
      f :: MV.MVector s Dynamic -> ST s ()
      f v = MV.modify v g tableId
      table' = V.modify f table
   in (ColumnID $ V.length table' - 1, Table table')
