#!/bin/bash

#apt-get update && apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack
#stack upgrade --git
#cp /root/.local/bin/stack /usr/bin/

git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
cd restful-payment-channel-server/
stack setup && stack build
