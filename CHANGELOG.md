# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

## [0.6.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.5.0.1..aztecs-v0.6.0) - 2025-2-19

## [0.5.0.1](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.5.0.0..aztecs-v0.5.0.1) - 2025-2-17

## Fixes

- Fix `Hackage` compilation with `examples` build flag (1b0655a)

## [0.5.0.0](https://github.com/aztecs-hs/aztecs/compare/aztecs-v0.4.0.1..aztecs-v0.5.0.0) - 2025-2-16

# Breaking changes

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
