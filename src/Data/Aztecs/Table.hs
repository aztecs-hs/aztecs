module Data.Aztecs.Table
  ( ColumnID (..),
    Column (..),
    Table (..),
    empty,
    replicate,
  )
where

import Data.Dynamic (Dynamic, Typeable, toDyn)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVector
import Prelude hiding (replicate)

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
