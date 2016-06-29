#!/bin/bash

set -e

apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack

git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
cd restful-payment-channel-server/

stack setup
stack build

