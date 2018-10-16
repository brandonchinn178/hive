#!/bin/bash
#
# Install Terraform

set -eux -o pipefail

is_command() {
    type "$1" &> /dev/null
}

case $(uname) in
    (Darwin) PLATFORM=darwin ;;
    (Linux)
        PLATFORM=linux
        yum install -y unzip
    ;;
esac

NAME=terraform_0.11.8_${PLATFORM}_amd64

if ! is_command terraform; then
    curl -LO "https://releases.hashicorp.com/terraform/0.11.8/${NAME}.zip"
    unzip "${NAME}.zip"
    rm "${NAME}.zip"
    mv terraform /usr/local/bin
fi

terraform --version
