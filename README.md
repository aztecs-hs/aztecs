# Aztecs

[![Discord](https://img.shields.io/discord/1306713440873877576.svg?label=&logo=discord&logoColor=ffffff&color=7389D8&labelColor=6A7EC2)](https://discord.gg/Hb7B3Qq4Xd)
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aztecs-hs/aztecs/blob/main/LICENSE)
[![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)
[![CI status](https://github.com/aztecs-hs/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/aztecs-hs/aztecs/actions)

A modular game engine and [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.
An ECS is a modern approach to organizing your application state as a database,
providing patterns for data-oriented design and parallel processing.

[Aztecs: An Empirical Entity Component System (ECS) for Haskell](https://github.com/aztecs-hs/paper) [Draft]

[Examples](https://github.com/aztecs-hs/examples)

## Features

- High-performance: Components are stored by their unique sets in archetypes
- Dynamic components: Scripts and remote interfaces can create runtime-specified components
- Type-safe DSL: Queries use `Applicative` syntax for compile-time gurantees
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Aztecs.ECS
import qualified Aztecs.ECS.World as W

newtype Position = Position Int

newtype Velocity = Velocity Int

move :: (PrimMonad m) => Query m (R Position, W m Velocity) -> m ()
move q = do
  results <- runQuery q
  mapM_ go results
  where
    go (R (Position x), vRef) = modifyW vRef (\(Velocity v) -> Velocity (v + x))

main :: IO ()
main = do
  w <- W.empty @_ @'[Position, Velocity]
  (e, w') <- W.spawn (Position 0) w
  w'' <- W.insert e (Velocity 1) w'
  move (W.query w'')
```

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
