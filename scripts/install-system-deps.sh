#!/bin/bash
#
# Install system dependencies.

set -eux -o pipefail

CABAL_BASE_URL=https://www.haskell.org/cabal/release/cabal-install-1.24.0.2
HERE=$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)

is_command() {
    type "$1" &> /dev/null
}

setup_darwin() {
    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-apple-darwin-yosemite.tar.gz
        curl "${CABAL_BASE_URL}/${CABAL_FILE}" | tar xz
        mv cabal /usr/local/bin/
    fi

    install_node darwin
    install_stack osx
    install_sass macos
}

setup_linux() {
    YUM_PACKAGES=(
        # stack
        automake
        gcc
        git
        gmp-devel
        gnupg
        libffi
        make
        perl
        tar
        xz
        zlib
        # stack deps
        zlib-devel
        # ghcjs
        ncurses-devel
    )
    if type sudo &> /dev/null; then
        CMD="sudo"
    else
        CMD="command"
    fi
    "$CMD" yum install -y "${YUM_PACKAGES[@]}"

    mkdir -p ~/.bin

    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz
        curl "${CABAL_BASE_URL}/${CABAL_FILE}" | tar xz --strip-components 7
        mv cabal /usr/local/bin/
    fi

    install_node linux
    install_stack linux
    install_sass linux
}

install_node() {
    local PLATFORM=$1
    # https://github.com/ghcjs/ghcjs/issues/668#issuecomment-414793423
    local NODE_VERSION=7.10.1
    local NODE="node-v${NODE_VERSION}-${PLATFORM}-x64"
    local DEST=/usr/local/lib/node

    if ! is_command node; then
        mkdir -p "$DEST"
        curl "https://nodejs.org/dist/v${NODE_VERSION}/${NODE}.tar.xz" | tar xJ -C "$DEST" --strip-components 1
        ln -sf "${DEST}/bin/node" /usr/local/bin/
        ln -sf "${DEST}/bin/npm" /usr/local/bin/
    fi

    node --version
    npm --version
}

install_stack() {
    local PLATFORM=$1

    if ! is_command stack; then
        local STACK="stack-1.7.1-${PLATFORM}-x86_64"
        curl -L "https://github.com/commercialhaskell/stack/releases/download/v1.7.1/${STACK}.tar.gz" | tar xz
        mv "${STACK}/stack" /usr/local/bin/
        rm -rf "${STACK}"
    fi

    stack --version
    stack setup
}

install_sass() {
    local PLATFORM=$1
    local SASS="dart-sass-1.14.3-${PLATFORM}-x64"
    local DEST=/usr/local/lib/sass

    if ! is_command sass; then
        mkdir -p "$DEST"
        curl -L "https://github.com/sass/dart-sass/releases/download/1.14.3/${SASS}.tar.gz" | tar xz -C "$DEST" --strip-components 1
        ln -sf "${DEST}/sass" /usr/local/bin/
    fi
}

case $(uname) in
    (Darwin) setup_darwin ;;
    (Linux) setup_linux ;;
esac
