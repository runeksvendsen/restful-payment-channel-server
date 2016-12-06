FROM ubuntu:16.04

RUN apt-get update && apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core haskell-stack jq

RUN stack setup --resolver lts-7.1

RUN git clone https://github.com/runeksvendsen/restful-payment-channel-server.git
RUN cd restful-payment-channel-server/
RUN stack install

COPY ./.stack-work/install/*/*/*/bin/* /usr/bin/
COPY test/* /usr/bin/

RUN mkdir -p /opt/paychan/state/test/

# CMD PORT=8080 /usr/bin/runEverything.sh /config/test
