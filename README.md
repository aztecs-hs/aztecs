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
import Control.Monad.IO.Class
import Data.Aztecs.Edit (Edit)
import qualified Data.Aztecs.Edit as C
import Data.Aztecs.Entity
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

app :: Edit IO ()
app = do
  C.spawn_ $ entity (Position 0) <&> Velocity 1
  C.spawn_ $ entity (Position 2) <&> Velocity 2

  positions <- Q.map $
    \(Position p :& Velocity v) -> Position (p + v)

  liftIO $ pPrint positions

main :: IO ()
main = do
  _ <- C.runEdit app W.empty
  return ()
```

## Benchmarks
Aztecs is currently faster than [bevy-ecs](https://github.com/bevyengine/bevy/), a popular and high-performance ECS written in Rust, for simple mutating queries.
<img alt="benchmark results: Aztecs 932us vs Bevy 6,966us" src="https://github.com/user-attachments/assets/348c7539-0e7b-4429-9cc1-06e8a819156d" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/)
