#!/bin/bash
#
# Builds the deployable artifacts.

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

ghcjs/stack.sh build "$@"

rm -rf build
mkdir -p build
cp "$(ghcjs/stack.sh path --local-install-root)/bin/hive.jsexe"/*.js build/
cp static/index.html build/
