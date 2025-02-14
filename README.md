# Aztecs

[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/matthunz/aztecs/blob/main/LICENSE)
[![CI status](https://github.com/matthunz/aztecs/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/aztecs/actions)

[Aztecs: An Empirical Entity Component System (ECS) for Haskell](https://github.com/matthunz/paper) [Draft]

A type-safe and friendly [ECS](https://en.wikipedia.org/wiki/Entity_component_system) for Haskell.
An ECS is a modern approach to organizing your application state as a database,
providing patterns for data-oriented design and parallel processing.
For more information, please see the documentation on [Hackage](https://hackage.haskell.org/package/aztecs/docs/Data-Aztecs.html).

## Features

- High-performance: Components are stored by their unique sets in archetypes
- Dynamic components: Scripts and remote interfaces can create runtime-specified components
- Type-safe DSL: Queries and systems use `Arrow` syntax for compile-time gurantees
- Modular design: Aztecs can be extended for a variety of use cases

## Packages

- [`aztecs`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs) [![Package](https://img.shields.io/hackage/v/aztecs.svg)](https://hackage.haskell.org/package/aztecs)
- [`aztecs-asset`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-asset) [![Package](https://img.shields.io/hackage/v/aztecs-asset.svg)](https://hackage.haskell.org/package/aztecs-asset)
- [`aztecs-hierarchy`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-hierarchy) [![Package](https://img.shields.io/hackage/v/aztecs-hierarchy.svg)](https://hackage.haskell.org/package/aztecs-hierarchy)
- [`aztecs-sdl`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-sdl) [![Package](https://img.shields.io/hackage/v/aztecs-sdl.svg)](https://hackage.haskell.org/package/aztecs-sdl)
- [`aztecs-transform`](https://github.com/matthunz/aztecs/blob/main/packages/aztecs-transform) [![Package](https://img.shields.io/hackage/v/aztecs-transform.svg)](https://hackage.haskell.org/package/aztecs-transform)

## Running examples

Examples can be run from the root folder of this repo with `cabal run {example}` (such as `cabal run ECS`).

## Benchmarks

Aztecs is currently faster than [bevy-ecs](https://github.com/bevyengine/bevy/), a popular and high-performance ECS written in Rust, for simple mutating queries.

<img alt="benchmark results: Aztecs 932us vs Bevy 6,966us" width=300 src="https://github.com/user-attachments/assets/d4b52548-0d68-428b-85d6-019428ecc0d0" />

## Inspiration

Aztecs' approach to type-safety is inspired by [Bevy](https://github.com/bevyengine/bevy/),
but with direct archetype-based storage similar to [Flecs](https://github.com/SanderMertens/flecs).
