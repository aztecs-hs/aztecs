module Aztecs.World.Entities where

import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word
import Aztecs.Entity

data Entities = Entities
  { entitiesNextGeneration :: Word32,
    entitiesGenerations :: IntMap Word32,
    entitiesNextIndex :: Word32,
    entitiesFreeIndicies :: [Word32]
  }

emptyEntities :: Entities
emptyEntities = Entities 0 IntMap.empty 0 []
{-# INLINE emptyEntities #-}

mkEntityWithCounter :: Entities -> (Entity, Entities)
mkEntityWithCounter (Entities gen gens index free) =
  let (i, nextIndex, free') = case free of
        (i' : rest) -> (i', index, rest)
        [] -> (index, index + 1, [])
      nextGeneration = gen + 1
      gens' = IntMap.insert (fromIntegral i) gen gens
   in (mkEntity i gen, Entities nextGeneration gens' nextIndex free')
{-# INLINE mkEntityWithCounter #-}

entities :: Entities -> [Entity]
entities (Entities _ gens _ _) = map go (IntMap.toList gens)
  where
    go (i, gen) = mkEntity (fromIntegral i) gen
{-# INLINE entities #-}
