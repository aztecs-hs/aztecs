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
newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

setup ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m
  ) =>
  m ()
setup = do
  e <- spawn
  A.insert e $ Position 0
  A.insert e $ Velocity 1

move ::
  ( MonadEntities m,
    MonadSystem (W (PrimState m) Position) m,
    MonadSystem (W (PrimState m) Velocity) m,
    MonadIO m,
    PrimMonad m
  ) =>
  m ()
move = do
  q <- runQuery $ (,,) <$> entities <*> query <*> query
  mapM_ go q
  where
    go (e, pRef, vRef) = do
      Velocity v <- readW vRef
      Position p <- readW pRef
      writeW pRef (Position $ p + v)

      p' <- readW pRef
      liftIO $ print (e, p')

main :: IO ()
main = do
  (((_, ps), vs), es) <- runEntitiesT (runAccessT (runAccessT setup S.empty) S.empty) emptyEntities
  vs' <- S.thaw vs
  ps' <- S.thaw ps
  _ <- runEntitiesT (runSystemT (runSystemT move vs') ps') es
  return ()
```

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
