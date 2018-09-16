#!/bin/bash
#
# Install third-party stack dependencies.

set -e

stack build --test --only-dependencies
ghcjs/stack.sh build --test --only-dependencies
stack build hlint stylish-haskell
