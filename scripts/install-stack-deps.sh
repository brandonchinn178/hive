#!/bin/bash
#
# Install third-party stack dependencies.

set -eo pipefail

stack build --test --only-dependencies
stack --stack-yaml stack-ghcjs.yaml build --test --only-dependencies
stack build hlint stylish-haskell
