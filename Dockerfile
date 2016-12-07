FROM ubuntu:16.04

RUN mkdir -p /opt/build/
COPY debian-build.sh /opt/build/

RUN cd /opt/build/; ./debian-build.sh

COPY .stack-work/install/*/*/*/bin/* /usr/bin/
COPY test/* /usr/bin/

RUN mkdir -p /opt/paychan/state/test/

# CMD PORT=8080 /usr/bin/runEverything.sh /config/test
