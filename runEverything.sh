#!/bin/bash

pkill SigningService
pkill ChanStore

set -e

NUMCORES=$2
PROFILE=""    # "-p"
RTSOPTS="+RTS -N$NUMCORES $PROFILE"
if [ -z $NUMCORES ]; then
   RTSOPTS="+RTS $PROFILE"
fi

echo "===== Using $NUMCORES core(s) ====="

SigningService "$1/config/signing.cfg" &
ChanStore "$1/config/store.cfg" ${RTSOPTS} &

sleep 0.2
PayChanServer "$1/config/server.cfg" ${RTSOPTS}


