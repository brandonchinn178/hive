#!/bin/bash
#
# Install system dependencies.

set -eu -o pipefail

is_command() {
    type "$1" &> /dev/null
}

setup_darwin() {
    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-apple-darwin-yosemite.tar.gz
        curl -O "https://www.haskell.org/cabal/release/cabal-install-1.24.0.2/${CABAL_FILE}"
        tar xf "$CABAL_FILE"
        rm -f "$CABAL_FILE"
        mv cabal /usr/local/bin/
    fi

    if ! is_command node; then
        if ! is_command brew; then
            echo "Install brew" >&2
            return 1
        fi
        brew install node
    fi
}

case $(uname) in
    (Darwin) setup_darwin ;;
esac
