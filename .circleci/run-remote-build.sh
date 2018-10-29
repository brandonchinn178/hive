#!/bin/bash

set -eux -o pipefail

export PATH=/usr/local/bin:$PATH

ghcjs/build.sh
tar czf build.tar.gz build/
