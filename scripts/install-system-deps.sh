#!/bin/bash
#
# Install system dependencies.

set -eu -o pipefail

CABAL_BASE_URL=https://www.haskell.org/cabal/release/cabal-install-1.24.0.2

is_command() {
    type "$1" &> /dev/null
}

setup_darwin() {
    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-apple-darwin-yosemite.tar.gz
        curl "${CABAL_BASE_URL}/${CABAL_FILE}" | tar xz
        mv cabal /usr/local/bin/
    fi

    if ! is_command node; then
        if ! is_command brew; then
            echo "Install brew" >&2
            return 1
        fi
        brew install node
    fi

    install_stack darwin
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
        # ghcjs
        ncurses-devel
    )
    yum install -y "${YUM_PACKAGES[@]}"

    mkdir -p ~/.bin

    if ! is_command cabal; then
        local CABAL_FILE=cabal-install-1.24.0.2-x86_64-unknown-linux.tar.gz
        curl "${CABAL_BASE_URL}/${CABAL_FILE}" | tar xz --strip-components 7
        mv cabal /usr/local/bin/
    fi

    if ! is_command node; then
        local NODE=node-v8.11.4-linux-x64
        curl "https://nodejs.org/dist/v8.11.4/${NODE}.tar.xz" | tar xJ
        mv "${NODE}/bin/node" /usr/local/bin/
        rm -rf "${NODE}"
    fi

    install_stack linux
}

install_stack() {
    local PLATFORM=$1

    if ! is_command stack; then
        local STACK="stack-1.7.1-${PLATFORM}-x86_64"
        curl -L "https://github.com/commercialhaskell/stack/releases/download/v1.7.1/${STACK}.tar.gz" | tar xz
        mv "${STACK}/stack" /usr/local/bin/
        rm -rf "${STACK}"
    fi

    stack setup

    if ! type alex &> /dev/null; then
        stack install alex --local-bin-path /usr/local/bin
    fi

    stack --stack-yaml stack-ghcjs.yaml setup
}

case $(uname) in
    (Darwin) setup_darwin ;;
    (Linux) setup_linux ;;
esac
