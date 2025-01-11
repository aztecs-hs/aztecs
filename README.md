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
import Data.Aztecs.Command (Command)
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

app :: Command IO ()
app = do
  C.spawn_ $ entity (Position 0) <&> Velocity 1
  C.spawn_ $ entity (Position 2) <&> Velocity 2

  positions <- Q.map @'[Position, Velocity] $ \e ->
    let (Position p) = component e
        (Velocity v) = component e
     in entity $ Position (p + v)

  liftIO $ pPrint positions

main :: IO ()
main = do
  _ <- C.runCommand app W.empty
  return ()
```

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/)
