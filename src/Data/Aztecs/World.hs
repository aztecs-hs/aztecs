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
import Data.Aztecs.World.Components (Components)
import qualified Data.Aztecs.World.Components as C
import Prelude hiding (lookup)

data World = World Components Entity
  deriving (Show)

empty :: World
empty = World C.empty (Entity 0)

spawn :: (Component c) => c -> World -> (Entity, World)
spawn c (World cs e) = (e, insert e c (World cs (Entity (unEntity e + 1))))

insert :: (Component c) => Entity -> c -> World -> World
insert e c (World cs e') = World (C.insert e c cs) e'

lookup :: (Component c) => Entity -> World -> Maybe c
lookup e (World cs _) = C.lookup e cs
