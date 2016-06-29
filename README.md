## RESTful Bitcoin payment channel server
### Server implementation of the [RESTful Bitcoin payment channel protocol](https://github.com/runeksvendsen/restful-payment-channel-server/wiki/Protocol-interface)

---

#### Build instructions
The following works with a fresh `ubuntu:16.04` docker image

    sudo apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack
    git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
    cd restful-payment-channel-server/
    stack setup && stack install

#### Stability
Experimental. Should work as intended, more or less, but there are several bugs and unhandled corner cases that I'm aware of, and probably some that I haven't found yet.

#### Documentation
Visit [test server](https://paychan.runeks.me).

#### Test server
https://paychan.runeks.me (also hosts documentation on the root path). Runs on Bitcoin testnet.

#### TODO
* ~~Auto-settlement~~
* OutPoint as key in chanMap

#### Build instructions
*TODO*

