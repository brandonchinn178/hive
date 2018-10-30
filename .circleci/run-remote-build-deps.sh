#!/bin/bash

set -eux -o pipefail

# unpack files
tar xf repo.tar.gz

if [[ -f ghcjs-cache.tar.gz ]]; then
    tar xf ghcjs-cache.tar.gz
    sudo mv usr-bin/* /usr/local/bin/
    sudo mv usr-lib/* /usr/local/lib/
    rm ghcjs-cache.tar.gz
fi

export PATH=/usr/local/bin:$PATH

sudo chown -R $USER:$USER /usr/local/
scripts/install-system-deps.sh
stack build alex happy
ghcjs/stack.sh setup
ghcjs/stack.sh build --only-dependencies

# pack cache
cp -r /usr/local/bin/ usr-bin
cp -r /usr/local/lib/ usr-lib
tar czf ghcjs-cache.tar.gz \
    .stack \
    .ghcjs \
    usr-bin \
    usr-lib
