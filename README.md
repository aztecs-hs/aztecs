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
import Control.Monad
import Control.Monad.IO.Class
import Data.Function

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int

instance Component Velocity

move :: Query Position
move = fetch & zipFetchMap (\(Velocity v) (Position p) -> Position $ p + v)

run :: SystemT IO ()
run = do
  positions <- query move
  liftIO $ print positions

app :: AccessT IO ()
app = do
  _ <- spawn $ bundle (Position 0) <> bundle (Velocity 1)
  forever $ system run

main :: IO ()
main = runAccessT_ app
```

## Scripting

[`aztecs-script`](https://github.com/aztecs-hs/aztecs-script) aims to be a turing-complete query language that can be used
for both scripting gameplay and controlling the ECS over a network.
This package provides both fully-typed Haskell DSL for scripting as well as a low-level interpreter for the text-based scripting language.

#### Haskell:

```hs
fetch @Position `as` #p <?> fetch @Velocity `as` #v `returning` #p :. #x :& #v :. #v
```

#### aztecs-script:

```sql
FETCH position AS p AND FETCH velocity AS v RETURNING (p.x, v.v)
```

## Packages

- [`aztecs-hierarchy`](https://github.com/aztecs-hs/aztecs-hierarchy)

  [![Package](https://img.shields.io/hackage/v/aztecs-hierarchy.svg)](https://hackage.haskell.org/package/aztecs-hierarchy)

  Parent-child hierarchies.

- [`aztecs-script`](https://github.com/aztecs-hs/aztecs-script)

  [![Package](https://img.shields.io/hackage/v/aztecs-script.svg)](https://hackage.haskell.org/package/aztecs-script)

  Aztecs scripting language.

- [`aztecs-sdl`](https://github.com/aztecs-hs/aztecs-sdl)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl.svg)](https://hackage.haskell.org/package/aztecs-sdl)

  SDL window management and rendering support.

- [`aztecs-sdl-image`](https://github.com/aztecs-hs/aztecs-sdl-image)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl-image.svg)](https://hackage.haskell.org/package/aztecs-sdl-image)

  SDL image and spritesheet rendering support.

- [`aztecs-sdl-text`](https://github.com/aztecs-hs/aztecs-sdl-text)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl-text.svg)](https://hackage.haskell.org/package/aztecs-sdl-text)

  SDL text rendering support.

## Benchmarks

<img width=300 alt="Benchmark results:  (Aztecs 160us) (Apecs 772us)
(Bevy	8us)" src="https://github.com/user-attachments/assets/2bd8603d-284e-4cd1-a6de-dba8df8a19cb"/>

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
