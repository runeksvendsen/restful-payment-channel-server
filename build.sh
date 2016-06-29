#!/bin/bash

apt-get install -y curl libssl-dev autoconf autogen libtool

curl -sSL https://get.haskellstack.org/ | sh

git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
cd restful-payment-channel-server/

stack setup
time stack build

ls .stack-work/install/x86_64-linux/lts-5.2/7.10.3/bin
