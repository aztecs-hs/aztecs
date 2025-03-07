# Aztecs

[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aztecs-hs/aztecs/blob/main/LICENSE)
[![CI status](https://github.com/aztecs-hs/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/aztecs-hs/aztecs/actions)
[![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)

A modular game engine and [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.
An ECS is a modern approach to organizing your application state as a database,
providing patterns for data-oriented design and parallel processing.

Aztecs provides side-effect free components and systems, as well as backends for rendering and input (such as `aztecs-sdl`), allowing you to structure your game as simple function of `Input -> World -> World`.
For more information, please see the documentation on [Hackage](https://hackage.haskell.org/package/aztecs/).

[Aztecs: An Empirical Entity Component System (ECS) for Haskell](https://github.com/aztecs-hs/paper) [Draft]

[Examples](https://github.com/aztecs-hs/examples)

## Features

- High-performance: Components are stored by their unique sets in archetypes
- Dynamic components: Scripts and remote interfaces can create runtime-specified components
- Type-safe DSL: Queries use `Applicative` syntax for compile-time gurantees
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Aztecs.ECS
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

move :: (Monad m) => QueryT m Position
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

## Packages

- [`aztecs-sdl`](https://github.com/aztecs-hs/aztecs-sdl)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl.svg)](https://hackage.haskell.org/package/aztecs-sdl)

  SDL window management and rendering support.

- [`aztecs-sdl-image`](https://github.com/aztecs-hs/aztecs-sdl-image)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl-image.svg)](https://hackage.haskell.org/package/aztecs-sdl-image)

  SDL image and spritesheet rendering support.

- [`aztecs-sdl-text`](https://github.com/aztecs-hs/aztecs-sdl-text)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl-text.svg)](https://hackage.haskell.org/package/aztecs-sdl-text)

  SDL text rendering support.

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
