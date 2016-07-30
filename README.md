# RESTful Bitcoin payment channel server
[![Build Status](https://api.travis-ci.org/runeksvendsen/restful-payment-channel-server.svg?branch=master)](https://travis-ci.org/runeksvendsen/restful-payment-channel-server)
#### Server implementation of the [RESTful Bitcoin Payment Channel Protocol](http://paychandoc.runeks.me/)
---

### What is this?
This server allows the operator to, after an initial setup step involving a sender/client, receive multiple, instant Bitcoin payments (no waiting for confirmations) from this pre-defined sender; paying the Bitcoin transaction fee only once, when the channel is closed.

So, a client contacts the server for funding information, pays to a funding address, waits the number of confirmations that the sever has specified, and then contacts the server to open the payment channel. Payments sent over the channel do not touch the Blockchain, so 1-Satoshi payments are fine, and as many can be sent without additional cost, until all the value has been used up. The channel can also be closed before that, in which case the client's unspent value is returned to its change address.

A payment channel also has a pre-defined expiration date. After this date, the client can reclaim all value sent over the channel, if it hasn't been closed yet. For the client, this means that opening a payment channel is trustless because, in case the server/receiver disappears, the client can just wait for the expiration date and reclaim funds. For the receiver, it means that the payment channel server must always be running if it has open payment channels. Not the least in order to be able to receive payments over these open channels in the first place, but also to be able to close channels before they reach expiration.

### Use cases
One could imagine, for example, setting up a service which sends video in response to payments, the first minute being free (if you have an open payment channel), and then charging, say 0.1 cent per second thereafter. Trustless pay-per-view. This server would only be used to verify payments, so an additional protocol, for sending video packets in response to payments, would have to be created.

### Stability
Under development. Mature enough to play with, but definitely not ready for production yet. Everything should work, but both the server data structures and interfaces can still change at any time.

### Bugs/questions
If you find a bug please create an issue here. If you need help, like if I haven't made it clear how to do something or just general support, creating an issue is fine too.

### Architecture overview
<img src="/doc/arch.png?raw=true" width="600">

### Build instructions
The following works with a fresh `ubuntu:16.04` docker image

    apt-get update && apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack
    git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
    cd restful-payment-channel-server/
    stack setup && stack build
    
For distributions without the `haskell-stack` package available, stack (the build tool) can be installed easily with a single command: [https://docs.haskellstack.org/en/stable/README/#how-to-install](https://docs.haskellstack.org/en/stable/README/#how-to-install).
    
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

### Testing
Setting the `debug.enable` config option to `true` disables any Blockchain querying. This includes both checking the funding address for funding, and publishing a settlement transaction to the Bitcoin network when the channel is closed. The funding information, that would otherwise have been queried from the Blockchain, is instead derived deterministically from the sender public key and expiration time: the *funding transaction hash* is set to the SHA256 hash of the client pubkey, the *vout*/*output index* is set to `expiration_timestamp % 7`, and the *value* is set to the constant **12345678900000**.

So, first, set the `debug.enable` option to `true` for the target server. Then the `benchPayChanServer.sh` script (located in `test/`) can be used, which executes a payment session (open, pay, close), performing the specified number of payments in the specified number of threads, using the debug-derived funding information. Usage:

    # First start server, in separate terminal (note pubkey):
    ./runEverything.sh config/debug/
    # Then execute test threads (1000 payments, 10 threads), passing the server pubkey as the last argument:
    ./benchPayChanServer.sh 1000 10 localhost:8000 0225b3aaf58992a8cc909522c2ec859ef218fd29fda0a6723cfb4e0529f80cc8f3

### Documentation
See [http://paychandoc.runeks.me/](http://paychandoc.runeks.me/).


### Performance
On my 2015 Macbook Pro I get ~900 payments per second running the `benchPayChanServer.sh` script:

    $ time ./benchPayChanServer.sh 2000 5 localhost:8000 0225b3aaf58992a8cc909522c2ec859ef218fd29fda0a6723cfb4e0529f80cc8f3
    [...]
    Waiting for clients to terminate...
    Done. Executed 10000 payments.
    
    real	0m10.962s
    user	0m6.211s
    sys     0m0.881s


### Live test servers
#### Bitcoin live net
[https://paychan.runeks.me](https://paychan.runeks.me/v1/fundingInfo?client_pubkey=03a67afebe772b05fcdf2a1f337bdaaf52343d62049793768d866b06194042e0cf&exp_time=1466539800)
#### Bitcoin testnet3
[https://paychantest.runeks.me](https://paychantest.runeks.me/v1/fundingInfo?client_pubkey=03a67afebe772b05fcdf2a1f337bdaaf52343d62049793768d866b06194042e0cf&exp_time=1466539800)

### TODO
* Local *bitcoind*-based address funding lookup
* Write test for expiration-based settlement
* Clean up imports
