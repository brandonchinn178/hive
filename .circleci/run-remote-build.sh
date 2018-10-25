#!/bin/bash

set -eux -o pipefail

# unpack files
tar xf repo.tar.gz

if [[ -f ghcjs-cache.tar.gz ]]; then
    tar xf ghcjs-cache.tar.gz
    mv usr-bin /usr/local/bin/
    mv usr-lib /usr/local/lib/
    rm ghcjs-cache.tar.gz
fi

export PATH=/usr/local/bin:$PATH
chown -R $USER:$USER .

scripts/install-system-deps.sh
scripts/install-stack-deps.sh
stack build alex happy
ghcjs/stack.sh setup
ghcjs/stack.sh build --only-dependencies

# pack cache
mv ~/.stack ~/.ghcjs .
mv /usr/local/bin/ usr-bin
mv /usr/local/lib/ usr-lib
tar czf ghcjs-cache.tar.gz \
    .stack \
    .ghcjs \
    usr-bin \
    usr-lib

ghcjs/build.sh
tar czf build.tar.gz build/

tar czf everything.tar.gz build.tar.gz ghcjs-cache.tar.gz
