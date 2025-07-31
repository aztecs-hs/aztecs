module Aztecs.ECS.Entities where

import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word

newtype Entity = Entity {unEntity :: Word64}
  deriving (Eq, Ord)

instance Show Entity where
  show e = "Entity {index = " ++ show (entityIndex e) ++ ", generation = " ++ show (entityGeneration e) ++ "}"

mkEntity :: Word32 -> Word32 -> Entity
mkEntity index generation = Entity $ (fromIntegral generation `shiftL` 32) .|. fromIntegral index

entityIndex :: Entity -> Word32
entityIndex (Entity e) = fromIntegral (e .&. 0xFFFFFFFF)

entityGeneration :: Entity -> Word32
entityGeneration (Entity e) = fromIntegral ((e `shiftR` 32) .&. 0xFFFFFFFF)

data Entities = Entities
  { entitiesNextGeneration :: Word32,
    entitiesGenerations :: IntMap Word32,
    entitiesNextIndex :: Word32,
    entitiesFreeIndicies :: [Word32]
  }

emptyEntities :: Entities
emptyEntities = Entities 0 IntMap.empty 0 []

mkEntityWithCounter :: Entities -> (Entity, Entities)
mkEntityWithCounter (Entities gen gens index free) =
  let (i, nextIndex, free') = case free of
        (i' : rest) -> (i', index, rest)
        [] -> (index, index + 1, [])
      nextGeneration = gen + 1
      gens' = IntMap.insert (fromIntegral i) gen gens
   in (mkEntity i gen, Entities nextGeneration gens' nextIndex free')

entities :: Entities -> [Entity]
entities (Entities _ gens _ _) = map go (IntMap.toList gens)
  where
    go (i, gen) = mkEntity (fromIntegral i) gen
