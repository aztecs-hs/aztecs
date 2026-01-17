# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

## [0.15.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.14.0..aztecs-v0.15.0) - 2026-1-17

## Breaking changes

- revert to `v0.9.0`-style archetypes but with new `Vector` storages

## [0.14.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.13.0..aztecs-v0.14.0) - 2025-8-10

## Breaking changes

- remove `f` wrapper from `HSet` by @matthunz in https://github.com/aztecs-hs/aztecs/pull/44
- make `W` generic over storages by @matthunz in https://github.com/aztecs-hs/aztecs/pull/47
- `HasDuplicateWrites` and refactor by @matthunz in https://github.com/aztecs-hs/aztecs/pull/48
- `Component` `Hooks` by @matthunz in https://github.com/aztecs-hs/aztecs/pull/49
- make `Component` hooks generic and restructure modules by @matthunz in https://github.com/aztecs-hs/aztecs/pull/50

# Refactors

- pass lints and fmt by @matthunz in https://github.com/aztecs-hs/aztecs/pull/51
- remove unused `Components` type family from ECS by @matthunz in https://github.com/aztecs-hs/aztecs/pull/52

## CI

- fix caching by @matthunz in https://github.com/aztecs-hs/aztecs/pull/45

## [0.13.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.12.1..aztecs-v0.13.0) - 2025-8-2

## Breaking changes

- rewrite with lower-level sparse sets by @matthunz in https://github.com/aztecs-hs/aztecs/pull/43

## [0.12.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.11.1..aztecs-v0.12.0) - 2025-3-14

## Breaking changes

- Remove requirement on `NFData` (435a8b5)
  - https://www.reddit.com/r/haskell/comments/1j57k50/comment/mgx23ad

- Convenience functions to run pure and applicative queries (such as `query` and `queryT`) (11b384b)

- Rename `Archetype.zipWith` and `Archetype.zipWithM` to `zipMap` and `zipMapM`, respectively (72990d0)

## [0.11.1](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.11.0..aztecs-v0.11.1) - 2025-3-7

## Documentation

- Fix Hackage compilation

## [0.11.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.10.0..aztecs-v0.11.0) - 2025-3-7

## Breaking changes

- Replace classes with interpreter pattern (77ed001) (a3c6db6) (9d9c4ca)
  - Combine query filters into queries (f1c9c55)
  - Query accum methods (ac3c0d8)
  - Concurrent systems (edaeffd)

- Remove engine-specific components
  - My plan is to move these to other packages down the road, focusing this package on a modular ECS

```hs
move :: (Monad m) => QueryT m Position
move = fetch & zipFetchMap (\(Velocity v) (Position p) -> Position $ p + v)
```

## [0.10.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.9.1..aztecs-v0.10.0) - 2025-3-6

## Breaking changes

- Applicative queries (5e8000f) (02c2028) (495c9e3)
  - Replaces the `Arrow` DSL with an applicative one, which I now believe has better ergonomics overall

## [0.9.1](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.9.0..aztecs-v0.9.1) - 2025-3-4

## Fixes

- Fix inserting bundles (40a13f6)

## Documentation

- Fix `since` version number (dbd71d2)

## [0.9.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.8.0..aztecs-v0.9.0) - 2025-3-4

## [0.8.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.7.0..aztecs-v0.8.0) - 2025-2-26

## Breaking changes

- Queue monadic tasks from systems (8e667f2)
- Rename query fields (403c7c3)
- Optimize `Archetype` storage (7c35b25) (cf3983c)
  - Entitiy IDs are now stored in each `Archetype`, with storages now only storing actual components

## Features

- Query conversion to-and-from readers (dfbc7a6)
- `allDyn` and `mapDyn` for running queries (78a0199)

## Fixes

- Special case for entity-only queries (35bccb0)

## Tests

- Property-based test for queries (e89c482)
- Update benchmark and remove potentially-false results (a895f68)

## [0.7.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.6.0..aztecs-v0.7.0) - 2025-2-22

## Breaking changes

- Aztecs is now split into several repos to avoid build conflicts
- Make `Asset.load` generic over its query (c1eab71)
- Make `Dynamic` system classes more generic (9cfe5e4)
- `ArrowQueueSystem` class (63b8135, ed9775a, 0753684)
- Generic schedule classes (e47a205, b45f8b6)
- Make `Transform` generic over its content, create `Transform2D` alias (687e7da)
- `MonoidBundle` class (03ab028)
- `ArrowChoice` and `ArrowLoop` instances for `Query` types (a6da1d9)
- `ArrowChoice` and `ArrowLoop` instances for `Schedule` types (88a0484)
- `ArrowChoice` and `ArrowLoop` instances for `System` types (8f6b356)
- Prune `World` and `Query` modules (c54d664)
- Split up `World` into new `Entities` type (a28f311)

## Features

- Propagate transforms (0f37b04)

## Fixes

- Remove `Parent` components from removed children (8370d71)

## [0.6.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.5.0.1..aztecs-v0.6.0) - 2025-2-19

## [0.5.0.1](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.5.0.0..aztecs-v0.5.0.1) - 2025-2-17

## Fixes

- Fix `Hackage` compilation with `examples` build flag (1b0655a)

## [0.5.0.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.4.0.1..aztecs-v0.5.0.0) - 2025-2-16

## Breaking changes

- New fully-parallel `System` and `Schedule` design (d51f8b0)
  - Still based on arrows, but `Schedule` now exists to explicitly apply any deferred `Access` in order.
- Strict by default (6a574f7) (7904a29)
- Bump minimum `base` (e79945d)

## [0.4.0.1](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.4.0.0...aztecs-v0.4.0.1) - 2025-2-13

## Features

- Add `World.lookup` (3d5c9c0)

## Fixes

- Inserting components into existing archetypes (ef9a3f1)
- Inserting components into empty entities and add hierarchy test (75a9710)

## Documentation

- Add link to paper (95a39aa)
- Update benchmark results (a8d7d95)

## Tests

- Setup QuickCheck (1848bb0)
- Add property tests (fb15d0f)
- Update benchmark (bfc8930)

## [0.4.0.0](https://github.com/aztecs-hs/aztecs/compare/v0.3.0.0..aztecs-v0.4.0.0) - 2025-2-7

## [0.3.0.0](https://github.com/aztecs-hs/aztecs/compare/v0.3.0.0..v0.4.0.0) - 2025-2-1

## [0.2.0.0](https://github.com/aztecs-hs/aztecs/compare/v0.1.0.1..v0.2.0.0) - 2025-1-24

## [0.1.0.1](https://github.com/aztecs-hs/aztecs/compare/v0.1.0.0..v0.1.0.1) - 2025-1-1

## [0.1.0.0](https://github.com/aztecs-hs/aztecs/compare/v0.1.0.0) - 2025-1-1
