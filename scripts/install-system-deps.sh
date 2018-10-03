#!/bin/bash
#
# Install system dependencies.

set -eux -o pipefail

CABAL_BASE_URL=https://www.haskell.org/cabal/release/cabal-install-1.24.0.2

is_command() {
    type "$1" &> /dev/null
}

setup_darwin() {
    mkdir -p "$DEST"

    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-apple-darwin-yosemite.tar.gz
        curl "${CABAL_BASE_URL}/${CABAL_FILE}" | tar xz
        mv cabal "$DEST"
    fi

    install_node darwin
    install_stack osx
}

setup_linux() {
    mkdir -p "$DEST"
    mkdir -p ~/.bin

    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz
        curl "${CABAL_BASE_URL}/${CABAL_FILE}" | tar xz --strip-components 7
        mv cabal "$DEST"
    fi

    install_node linux
    install_stack linux
}

install_node() {
    local PLATFORM=$1
    # https://github.com/ghcjs/ghcjs/issues/668#issuecomment-414793423
    local NODE_VERSION=7.10.1
    local NODE="node-v${NODE_VERSION}-${PLATFORM}-x64"
    local NODE_DEST=/usr/local/lib/node

    if ! is_command node; then
        mkdir -p "$NODE_DEST"
        curl "https://nodejs.org/dist/v${NODE_VERSION}/${NODE}.tar.xz" | tar xJ -C "$NODE_DEST" --strip-components 1
        ln -sf "${NODE_DEST}/bin/node" "$DEST"
        ln -sf "${NODE_DEST}/bin/npm" "$DEST"
    fi

    node --version
    npm --version
}

install_stack() {
    local PLATFORM=$1

    if ! is_command stack; then
        local STACK="stack-1.7.1-${PLATFORM}-x86_64"
        curl -L "https://github.com/commercialhaskell/stack/releases/download/v1.7.1/${STACK}.tar.gz" | tar xz
        mv "${STACK}/stack" "$DEST"
        rm -rf "${STACK}"
    fi

    stack --version
    stack setup

    # Tools needed for GHCJS
    stack build alex happy

    ghcjs/stack.sh setup
}

case $(uname) in
    (Darwin)
        DEST=/usr/local/bin
        setup_darwin
    ;;
    (Linux)
        DEST=~/bin
        setup_linux
    ;;
esac
