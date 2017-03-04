FROM fpco/stack-build

RUN apt-get update && apt-get install -y libssl-dev autoconf autogen libtool xz-utils git-core

## Increase max open files limit
RUN echo "*               soft    nofile            10240" >> /etc/security/limits.conf
RUN echo "*               hard    nofile            10240" >> /etc/security/limits.conf
RUN echo "fs.file-max = 100000"                            >> /etc/sysctl.conf


# CMD PORT=8080 /usr/bin/runEverything.sh /config/test
