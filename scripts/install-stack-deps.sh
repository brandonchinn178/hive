#!/bin/bash
#
# Install third-party stack dependencies.

set -eo pipefail

./build.sh --test --only-dependencies
stack build hlint stylish-haskell
