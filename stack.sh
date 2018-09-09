#!/bin/bash
#
# Runs 'stack' as appropriate for 'stack.yaml'

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

exec stack --stack-yaml stack.yaml "$@"
