# Aztecs

[![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/matthunz/aztecs/blob/main/LICENSE)
[![CI status](https://github.com/matthunz/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/aztecs/actions)

A type-safe and friendly [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.

## Features

- High-performance: Components are stored by their unique sets in vector-based archetypes
- Dynamic components: Scripts and remote interfaces can create unique components with runtime-specified components
- Type-safe DSL: Components and systems are accessed by marker types that determine their storage
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

app :: Access IO ()
app = do
  -- Spawn an entity with position and velocity components
  e <- A.spawn (Position 0)
  A.insert e (Velocity 1)

  -- Update all matching entities
  q <- Q.map (\(Position x :& Velocity v) -> Position (x + v))
  liftIO $ print q

main :: IO ()
main = do
  _ <- runAccess app W.empty
  return ()
```

## Benchmarks
Aztecs is currently faster than [bevy-ecs](https://github.com/bevyengine/bevy/), a popular and high-performance ECS written in Rust, for simple mutating queries.

<img alt="benchmark results: Aztecs 932us vs Bevy 6,966us" width=300 src="https://github.com/user-attachments/assets/348c7539-0e7b-4429-9cc1-06e8a819156d" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
