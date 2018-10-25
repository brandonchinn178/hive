#!/bin/bash

set -eux -o pipefail

ghcjs/build.sh
tar czf build.tar.gz build/
