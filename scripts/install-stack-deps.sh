#!/bin/bash
#
# Install third-party stack dependencies.

set -e

# prevent out of memory
stack build haskell-src-exts
stack build jsaddle-dom

stack build --test --only-dependencies
stack build hlint stylish-haskell
