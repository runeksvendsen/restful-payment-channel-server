## RESTful Bitcoin payment channel server
### Server implementation of the [RESTful Bitcoin payment channel protocol](http://paychandoc.runeks.me/)

---

#### Build instructions
The following works with a fresh `ubuntu:16.04` docker image

    sudo apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack
    git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
    cd restful-payment-channel-server/
    stack setup && stack build
    
#### Running
The server executable has one required argument: the path to the config file. Example config files can be found in `config/` as a `server.cfg` file for both Bitcoin livenet and testnet3. The file extension of the config file is ignored (`.cfg` in this case).

Command to run the server with a configuration contained in the file `/etc/paychan/live.cfg`:

    PayChanServer /etc/paychan/live.cfg

#### Stability
Experimental. Should work as intended, more or less, but there are several bugs and unhandled corner cases that I'm aware of, and probably some that I haven't found yet.

#### Documentation
See [http://paychandoc.runeks.me/](http://paychandoc.runeks.me/).

#### Running hosts
##### Live server
https://paychan.runeks.me. Runs on livenet.
##### Test server
https://paychantest.runeks.me. Runs on Bitcoin testnet.

#### TODO

##### Soon
* Actually close channel before expiration date
* OutPoint as key in chanMap
* ~~Sync chanMap to disk on TERM signal~~

##### Less soon
* Split server into three: 1) logic front-end 2) open channel store backend (database) 3) settlement service

