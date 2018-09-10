#!/bin/bash
#
# Install third-party stack dependencies.

set -eo pipefail

./build-all.sh --test --only-dependencies
./stack.sh build hlint stylish-haskell
