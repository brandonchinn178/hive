#!/bin/bash
#
# Runs `stack` with `ghcjs`.

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

export PATH=$PATH:$(stack path --snapshot-install-root)/bin:$(stack path --compiler-bin)

if [[ "${NO_GHCJS:-}" != "true" ]]; then
    stack --stack-yaml=ghcjs/stack.yaml "$@"
fi
