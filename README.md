# Hive

A Haskell implementation of the Hive game.

https://boardgamegeek.com/boardgame/2655/hive

## Quickstart

1. `scripts/install-system-deps.sh`
1. `scripts/install-stack-deps.sh`
1. `stack build`
1. `stack exec hive`
1. Go to `http://localhost:3000`

## Building

The project is set up to work for both `ghc` and `ghcjs`. It's faster to
develop with `ghc`, so just use `stack build` to build and
`stack exec hive` to start running the server.

To run stack with `ghcjs`, use the `ghcjs/stack.sh` script. To imitate a
deployment build, run `ghcjs/build.sh`, which will compile the static files
to `build/`.

## Linting

1. `scripts/hlint.sh`
1. `scripts/stylish-haskell.sh [--apply]`
