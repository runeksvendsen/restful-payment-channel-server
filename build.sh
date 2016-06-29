#!/bin/bash

set -e

apt-get install -y curl libssl-dev autoconf autogen libtool haskell-stack

git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
cd restful-payment-channel-server/

stack setup
stack install

ls /root/.local/bin
