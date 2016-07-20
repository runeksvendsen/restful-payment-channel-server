# RESTful Bitcoin payment channel server
## Server implementation of the [RESTful Bitcoin payment channel protocol](http://paychandoc.runeks.me/)

---

### Build instructions
The following works with a fresh `ubuntu:16.04` docker image

    sudo apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack libleveldb-dev
    git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
    cd restful-payment-channel-server/
    stack setup && stack build
    
### Running
The server executable has one required argument: the path to the config file. Example config files can be found in `config/` as a `server.cfg` file for both Bitcoin livenet and testnet3. Example:

    PayChanServer /etc/paychan/live.cfg
    
Pass the desired listening port to the server via the `PORT` environment variable:

    PORT=43617 PayChanServer /home/rune/paychan/test.cfg 

### Stability
Experimental. Should work as intended, more or less, but there are some bugs and unhandled corner cases that I'm aware of, and perhaps some that I haven't found yet.

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

