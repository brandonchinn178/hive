#!/bin/bash
#
# Runs 'stack' as appropriate for 'stack-ghcjs.yaml'

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

stack_ghcjs() {
    stack --stack-yaml stack-ghcjs.yaml --stack-root ~/.stack-ghcjs "$@"
}

stack_ghcjs "$@"

CLIENT_ROOT=$(stack_ghcjs path --local-install-root)
CLIENT_SRC="${CLIENT_ROOT}/bin/hive-client.jsexe"
if [[ -d "$CLIENT_SRC" ]]; then
    mkdir -p server/static
    cp "${CLIENT_SRC}"/*.js server/static/
fi
