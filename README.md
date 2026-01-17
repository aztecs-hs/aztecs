# Aztecs

[![Discord](https://img.shields.io/discord/1306713440873877576.svg?label=&logo=discord&logoColor=ffffff&color=7389D8&labelColor=6A7EC2)](https://discord.gg/Hb7B3Qq4Xd)
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aztecs-hs/aztecs/blob/main/LICENSE)
[![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)
[![CI status](https://github.com/aztecs-hs/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/aztecs-hs/aztecs/actions)

A modular game engine and [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.
An ECS is a modern approach to organizing your application state as a database,
providing patterns for data-oriented design and parallel processing.

[Examples](https://github.com/aztecs-hs/aztecs/tree/main/examples)

## Features
- Type-safe: Queries and systems use fully type-checked access with compile-time gurantees
- High-performance: Components are stored by their unique sets in archetypes
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Monad.IO.Class
import Data.Function ((&))

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

move :: QueryT IO Position
move = Q.fetch & Q.adjust (\(Velocity v) (Position p) -> Position $ p + v)

run :: AccessT IO ()
run = do
  positions <- S.map move
  liftIO $ print positions

app :: AccessT IO ()
app = do
  A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
  run

main :: IO ()
main = runAccessT_ app
```

## Prior art

Aztecs' approach to archetypical ECS is inspired by [Bevy](https://github.com/bevyengine/bevy/) and [Flecs](https://github.com/SanderMertens/flecs).

A fantastic lower-level (but higher-performance) Haskell ECS [Apecs](https://github.com/jonascarpay/apecs)
