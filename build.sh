#!/bin/bash
#
# With no arguments, builds everything. Takes in the usual stack arguments,
# except instead of the usual stack targets, the available stack targets
# are: lib, server, client.

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")"

ARGS=()

BUILD_ALL=1
BUILD_LIB=0
BUILD_SERVER=0
BUILD_CLIENT=0

while [[ $# -gt 0 ]]; do
    case $1 in
        (lib)    BUILD_ALL=0; BUILD_LIB=1 ;;
        (server) BUILD_ALL=0; BUILD_SERVER=1 ;;
        (client) BUILD_ALL=0; BUILD_CLIENT=1 ;;
        (*)      ARGS+=("$1") ;;
    esac
    shift
done

BUILD_LIB=$(( BUILD_ALL || BUILD_LIB ))
BUILD_SERVER=$(( BUILD_ALL || BUILD_SERVER ))
BUILD_CLIENT=$(( BUILD_ALL || BUILD_CLIENT ))

if (( BUILD_CLIENT )); then
    stack build --stack-yaml stack-ghcjs.yaml hive-client "${ARGS[@]}"

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
    stack build "${STACK_ARGS[@]}" "${ARGS[@]}"
fi
