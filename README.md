## RESTful Bitcoin payment channel server
### Server implementation of the [RESTful Bitcoin payment channel protocol](http://paychandoc.runeks.me/)

---

#### Build instructions
The following works with a fresh `ubuntu:16.04` docker image

    sudo apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack
    git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
    cd restful-payment-channel-server/
    stack setup && stack build

#### Stability
Experimental. Should work as intended, more or less, but there are several bugs and unhandled corner cases that I'm aware of, and probably some that I haven't found yet.

#### Documentation
See [http://paychandoc.runeks.me/](http://paychandoc.runeks.me/).

#### Running hosts
##### Live server
https://paychan.runeks.me. Runs on livenet.
##### Test server
http://paychantest.runeks.me. Runs on Bitcoin testnet.

#### TODO
* ~~Auto-settlement~~
* OutPoint as key in chanMap

