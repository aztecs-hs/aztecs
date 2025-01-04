# Aztecs

[![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)
[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/matthunz/aztecs/blob/main/LICENSE)
[![CI status](https://github.com/matthunz/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/aztecs/actions)

A type-safe and friendly [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.

## Features

- High-performance: Queries use archetypes to efficiently access the ECS
- Automatic parallelism: Systems run in parallel if possible, and in multiple stages
- Type-safe DSL: Components and systems are accessed by marker types that determine their storage
- Modular design: Aztecs can be extended for a variety of use cases

```hs
import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S

-- Components

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

-- Systems

data A

instance System IO A where
  access = S.command $ do
    e <- C.spawn (Position 0)
    C.insert e (Velocity 1)

data B

instance System IO B where
  access = do
    -- Update all entities with a `Position` and `Velocity` component.
    positions <- S.all $ do
      Velocity v <- Q.read
      Q.write (\(Position p) -> Position (p + v))

    liftIO $ print positions

app :: Scheduler IO
app = schedule @Startup @_ @A [] <> schedule @Update @_ @B []

main :: IO ()
main = runScheduler app
```

## Benchmarks
Aztecs currently beats the Bevy ECS for simple iterations.

<img width=300 alt="Benchmark results" src="https://github.com/user-attachments/assets/7264a08a-a04b-4217-86a7-27f4b7fefe54" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/)
