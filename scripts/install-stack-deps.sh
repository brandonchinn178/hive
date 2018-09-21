#!/bin/bash
#
# Install third-party stack dependencies.

set -e

if [[ "${NO_GHC:-}" != "true" ]]; then
    stack build --test --only-dependencies
fi

# needs to be installed explicitly first for linux
ghcjs/stack.sh build ghcjs-dom-jsffi
ghcjs/stack.sh build --test --only-dependencies
stack build hlint stylish-haskell
