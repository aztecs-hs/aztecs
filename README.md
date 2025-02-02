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
import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.System as S

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data Setup

instance System IO Setup where
  task = S.queue (A.spawn_ (Position 0 :& Velocity 1))

data Movement

instance System IO Movement where
  task = S.map (\(Position x :& Velocity v) -> Position (x + v)) >>> S.run print

app :: Scheduler IO
app = schedule @_ @Startup @Setup [] <> schedule @_ @Update @Movement []

main :: IO ()
main = run app
```

## SDL
```hs
import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import Data.Aztecs.SDL (Image (..), Window (..), sdlPlugin)
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (V2 (..))

data Setup

instance System IO Setup where
  task =
    S.mapSingleWith (\assetServer -> load "example.png" assetServer)
      >>> S.queueWith
        ( \(texture, _) -> do
            A.spawn_ Window {windowTitle = "Aztecs"}
            A.spawn_ $
              Image {imageTexture = texture, imageSize = V2 100 100}
                :& transform {transformPosition = V2 100 100}
            A.spawn_ $
              Image {imageTexture = texture, imageSize = V2 200 200}
                :& transform {transformPosition = V2 500 100}
        )

app :: Scheduler IO
app = sdlPlugin <> schedule @_ @Startup @Setup []

main :: IO ()
main = run app
```

## Benchmarks

Aztecs is currently faster than [bevy-ecs](https://github.com/bevyengine/bevy/), a popular and high-performance ECS written in Rust, for simple mutating queries.

<img alt="benchmark results: Aztecs 932us vs Bevy 6,966us" width=300 src="https://github.com/user-attachments/assets/348c7539-0e7b-4429-9cc1-06e8a819156d" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
