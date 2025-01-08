module Data.Aztecs.World
  ( World (..),
    empty,
    spawn,
    insert,
    lookup,
  )
where

import Data.Aztecs.Component (Component)
import Data.Aztecs.Core (Entity (..))
import Data.Aztecs.World.Archetypes (Archetypes)
import qualified Data.Aztecs.World.Archetypes as A
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as C
import Prelude hiding (lookup)

data World = World Components Archetypes Entity
  deriving (Show)

empty :: World
empty = World C.empty A.empty (Entity 0)

spawn :: (Component c) => c -> World -> (Entity, World)
spawn c (World cs as e) = (e, insert e c (World cs as (Entity (unEntity e + 1))))

insert :: (Component c) => Entity -> c -> World -> World
insert e c (World cs as e') =
  let (cId, r, cs') = C.insert e c cs
   in World cs' (A.insert e cId r cs as) e'

lookup :: (Component c) => Entity -> World -> Maybe c
lookup e (World cs _ _) = C.lookup e cs
