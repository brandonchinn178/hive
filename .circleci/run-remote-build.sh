#!/bin/bash

set -eu -o pipefail

ls

if [[ -f ghcjs-cache.tar.gz ]]; then
    tar tf ghcjs-cache.tar.gz
fi
