# RESTful Bitcoin payment channel server
[![Build Status](https://api.travis-ci.org/runeksvendsen/restful-payment-channel-server.svg?branch=master)](https://travis-ci.org/runeksvendsen/restful-payment-channel-server)
#### Server implementation of the [RESTful Bitcoin Payment Channel Protocol](http://paychandoc.runeks.me/)
---

### Architecture overview
<img src="/doc/arch.png?raw=true" width="600">

### Build instructions
The following works with a fresh `ubuntu:16.04` docker image

    apt-get update && apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack libleveldb-dev
    git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
    cd restful-payment-channel-server/
    stack setup && stack build
    
### Running
The payment channel server consists of three executables:

* `PayChanServer` (client facing; implements payment channel protocol) `server.cfg`
* `ChanStore` (database; stores open payment channel states) `store.cfg`
* `SigningService` (private key custodian; signs Bitcoin settlement transactions) `signing.cfg`
    
Each executable takes a single required argument: the path to its config file.

Example config files can be found in `config/`, which has config files for Bitcoin livenet, testnet, and a debug configuration which does not reach out to the Bitcoin network. See `config/live/`, `config/test/` and `config/debug/`, respectively.
    
For `PayChanServer`, you can set the desired listening port via the `PORT` environment variable. Eg.:

    PORT=43617 PayChanServer config/live/config/server.cfg
    
The `runEverything.sh` script runs all three components, taking as argument the root path of the config you want to use, eg. `config/debug/`:

    ./runEverything.sh config/debug/

### Stability
Under development.

### Documentation
See [http://paychandoc.runeks.me/](http://paychandoc.runeks.me/).

### Live test servers
#### Bitcoin live net
[https://paychan.runeks.me](https://paychan.runeks.me/v1/fundingInfo?client_pubkey=03a67afebe772b05fcdf2a1f337bdaaf52343d62049793768d866b06194042e0cf&exp_time=1466539800)
#### Bitcoin testnet3
[https://paychantest.runeks.me](https://paychantest.runeks.me/v1/fundingInfo?client_pubkey=03a67afebe772b05fcdf2a1f337bdaaf52343d62049793768d866b06194042e0cf&exp_time=1466539800)

### Performance
On my 2015 Macbook Pro I get 800-900 payments per second running the `benchPayChanServer.sh` script (located in `test/`):

    $ time ./benchPayChanServer.sh 2000 5 localhost:8000
    Spawning client 1
    Spawning client 2
    [...]
    Done. Executed 10000 payments.
    real	0m11.194s
    user	0m6.651s
    sys     0m0.917s

This performs 2000 payments using 5 concurrent threads.

### TODO

* Actually close channel before expiration date (write settlement service)
* ~~OutPoint as key in chanMap~~

