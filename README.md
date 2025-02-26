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
- Type-safe DSL: Queries and systems use `Arrow` syntax for compile-time gurantees
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Arrow
import Control.DeepSeq
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

setup :: (ArrowQueueSystem b m arr) => arr () ()
setup = S.queue . const . A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)

move :: (ArrowQuery q, ArrowSystem q arr) => arr () [Position]
move =
  S.map
    ( proc () -> do
        Velocity v <- Q.fetch -< ()
        Position p <- Q.fetch -< ()
        Q.set -< Position $ p + v
    )

app :: Schedule IO () ()
app = system setup >>> forever (system move) print

main :: IO ()
main = runSchedule_ app
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
