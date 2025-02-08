# Aztecs

[![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/matthunz/aztecs/blob/main/LICENSE)
[![CI status](https://github.com/matthunz/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/aztecs/actions)

[Aztecs: An Empirical Entity Component System (ECS) for Haskell](https://github.com/matthunz/paper) [Draft]

A type-safe and friendly [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.
An ECS is a modern approach to organizing your application state as a database,
providing patterns for data-oriented design and parallel processing.
For more information, please see the documentation on [Hackage](https://hackage.haskell.org/package/aztecs/docs/Data-Aztecs.html).

## Features

- High-performance: Components are stored by their unique sets in archetypes
- Dynamic components: Scripts and remote interfaces can create unique components with runtime-specified components
- Type-safe DSL: Queries and systems use `Arrow` syntax for compile-time gurantees
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

setup :: System () ()
setup = S.queue . const . A.spawn_ $ bundle (Position 0) <> bundle (Velocity 1)

move :: System () ()
move =
  S.map
    ( proc () -> do
        Velocity v <- Q.fetch -< ()
        Position p <- Q.fetch -< ()
        Q.set -< Position $ p + v
    )
    >>> S.run print

main :: IO ()
main = runSystem_ $ setup >>> S.forever move
```

## SDL
```hs
import Control.Arrow (returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Image (..), Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (V2 (..))

setup :: System () ()
setup =
  S.mapSingle
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run (load "example.png") -< assetServer
        Q.set -< assetServer'
        returnA -< texture
    )
    >>> S.queue
      ( \texture -> do
          A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
          A.spawn_ $
            bundle Image {imageTexture = texture, imageSize = V2 100 100}
              <> bundle transform {transformPosition = V2 100 100}
          A.spawn_ $
            bundle Image {imageTexture = texture, imageSize = V2 200 200}
              <> bundle transform {transformPosition = V2 500 100}
      )

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.forever SDL.update
```

## Benchmarks

Aztecs is currently faster than [bevy-ecs](https://github.com/bevyengine/bevy/), a popular and high-performance ECS written in Rust, for simple mutating queries.

<img alt="benchmark results: Aztecs 932us vs Bevy 6,966us" width=300 src="https://github.com/user-attachments/assets/348c7539-0e7b-4429-9cc1-06e8a819156d" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
