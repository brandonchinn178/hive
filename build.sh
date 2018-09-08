#!/bin/bash
#
# With no arguments, builds everything. Can also take in one or more
# projects to build.

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

USAGE='"
Usage: $0 [PROJECT ..]

PROJECT: lib, server, client
"'

if [[ $# == 0 ]]; then
    BUILD_LIB=1
    BUILD_SERVER=1
    BUILD_CLIENT=1
else
    BUILD_LIB=0
    BUILD_SERVER=0
    BUILD_CLIENT=0
    while [[ $# -gt 0 ]]; do
        BUILD_ALL=0
        case $1 in
            (lib)    BUILD_LIB=1 ;;
            (server) BUILD_SERVER=1 ;;
            (client) BUILD_CLIENT=1 ;;
            (*)
                eval echo "$USAGE" >&2
                exit 1
            ;;
        esac
        shift
    done
fi

if (( BUILD_CLIENT )); then
    stack build --stack-yaml stack-ghcjs.yaml hive-client

    CLIENT_ROOT=$(stack path --stack-yaml stack-ghcjs.yaml --local-install-root)
    mkdir -p server/static
    cp "${CLIENT_ROOT}/bin/hive-client.jsexe"/*.js server/static/
fi

if (( BUILD_LIB || BUILD_SERVER )); then
    STACK_ARGS=()
    if (( BUILD_LIB )); then
        STACK_ARGS+=(hive)
    fi
    if (( BUILD_SERVER )); then
        STACK_ARGS+=(hive-server)
    fi
    stack build "${STACK_ARGS[@]}"
fi
