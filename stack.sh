#!/bin/bash
#
# Runs 'stack' as appropriate for 'stack.yaml'

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

export STATIC_DIR=$PWD/server/static

exec stack --stack-yaml stack.yaml "$@"
