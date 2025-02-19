# Aztecs

[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/matthunz/aztecs/blob/main/LICENSE)
[![CI status](https://github.com/matthunz/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/aztecs/actions)

[Aztecs: An Empirical Entity Component System (ECS) for Haskell](https://github.com/matthunz/paper) [Draft]

A type-safe and friendly [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.
An ECS is a modern approach to organizing your application state as a database,
providing patterns for data-oriented design and parallel processing.
For more information, please see the documentation on [Hackage](https://hackage.haskell.org/package/aztecs/docs/Data-Aztecs.ECS.html).

## Features

- High-performance: Components are stored by their unique sets in archetypes
- Dynamic components: Scripts and remote interfaces can create runtime-specified components
- Type-safe DSL: Queries and systems use `Arrow` syntax for compile-time gurantees
- Modular design: Aztecs can be extended for a variety of use cases

## Packages

- [`aztecs`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs)

  [![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)

  Modular game engine and ECS.

- [`aztecs-sdl`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-sdl)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl.svg)](https://hackage.haskell.org/package/aztecs-sdl)

  SDL window management and rendering support.

- [`aztecs-sdl-image`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-sdl-image)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl-image.svg)](https://hackage.haskell.org/package/aztecs-sdl-image)

  SDL image and spritesheet rendering support.

- [`aztecs-sdl-text`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-sdl-text)

  [![Package](https://img.shields.io/hackage/v/aztecs-sdl-text.svg)](https://hackage.haskell.org/package/aztecs-sdl-text)

  SDL text rendering support.

## Running examples

Examples can be run from the root folder of this repo with `cabal run {example} -f examples` (such as `cabal run ECS -f examples`).

## Benchmarks

Aztecs is currently faster than [bevy-ecs](https://github.com/bevyengine/bevy/), a popular and high-performance ECS written in Rust, for simple mutating queries.

<img alt="benchmark results: Aztecs 932us vs Bevy 6,966us" width=300 src="https://github.com/user-attachments/assets/d4b52548-0d68-428b-85d6-019428ecc0d0" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
