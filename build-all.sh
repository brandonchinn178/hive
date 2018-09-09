#!/bin/bash

set -eu -o pipefail

./stack-ghcjs.sh build "$@"
./stack.sh build "$@"
