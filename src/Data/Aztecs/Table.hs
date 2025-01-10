{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Table
  ( ColumnID (..),
    Column (..),
    lookupColumnId,
    TableID (..),
    Table (..),
    singleton,
    singletonDyn,
    length,
    lookup,
    lookupDyn,
    lookupColumn,
    cons,
    insert,
    remove,
    removeDyn,
    toList,
  )
where

import Control.Monad.ST
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Prelude hiding (length, lookup, replicate)

newtype ColumnID = ColumnID {unColumnId :: Int}
  deriving (Eq, Ord, Show)

newtype Column = Column (Vector Dynamic) deriving (Show)

lookupColumnId :: (Typeable c) => ColumnID -> Column -> Maybe c
lookupColumnId (ColumnID colId) (Column col) = col V.!? colId >>= fromDynamic

newtype TableID = TableID {unTableId :: Int}
  deriving (Eq, Ord, Show)

newtype Table = Table (Vector Column)
  deriving (Show, Semigroup, Monoid)

singleton :: (Typeable c) => c -> Table
singleton = singletonDyn . toDyn

singletonDyn :: Dynamic -> Table
singletonDyn c = Table . V.singleton . Column $ V.singleton c

length :: Table -> Int
length (Table t) = V.length t

lookup :: (Typeable c) => TableID -> Table -> ColumnID -> Maybe c
lookup tId t colId = lookupDyn tId colId t >>= fromDynamic

lookupDyn :: TableID -> ColumnID -> Table -> Maybe Dynamic
lookupDyn tId (ColumnID colId) t = do
  Column col <- lookupColumn tId t
  col V.!? colId

lookupColumn :: TableID -> Table -> Maybe Column
lookupColumn (TableID tableId) (Table table) = table V.!? tableId

remove :: (Typeable c) => TableID -> ColumnID -> Table -> Maybe (c, Table)
remove (TableID tableId) (ColumnID colId) (Table table) = do
  Column col <- table V.!? tableId
  let (left, right) = V.splitAt colId col
      dyn = V.head right
      col' = left V.++ V.tail right
  c <- fromDynamic dyn
  return (c, Table (table V.// [(tableId, Column col')]))

removeDyn :: TableID -> ColumnID -> Table -> Maybe (Dynamic, Table)
removeDyn (TableID tableId) (ColumnID colId) (Table table) = do
  Column col <- table V.!? tableId
  let (left, right) = V.splitAt colId col
      dyn = V.head right
      col' = left V.++ V.tail right
  return (dyn, Table (table V.// [(tableId, Column col')]))

insert :: (Typeable c) => TableID -> ColumnID -> c -> Table -> Table
insert (TableID tableId) (ColumnID colId) c (Table table) =
  let h v = MV.write v colId (toDyn c)
      g (Column col) = Column (V.modify h col)
      f :: MV.MVector s Column -> ST s ()
      f v = MV.modify v g tableId
   in Table $ V.modify f table

cons :: (Typeable c) => TableID -> c -> Table -> Table
cons (TableID tableId) c (Table table) = Table $ table V.// [(tableId, Column (V.cons (toDyn c) col))]
  where
    Column col = table V.! tableId

toList :: Table -> [Column]
toList (Table t) = V.toList t