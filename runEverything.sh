#!/bin/bash

pkill SigningService
pkill ChanStore

set -e

NUMCORES=$2
RTSOPTS="+RTS -N$NUMCORES"
if [ -z $NUMCORES ]; then
   RTSOPTS=""
fi

echo "===== Using $NUMCORES core(s) ====="

SigningService "$1/config/signing.cfg" > /dev/null &
ChanStore "$1/config/store.cfg" ${RTSOPTS} > /dev/null &

sleep 0.2
PayChanServer "$1/config/server.cfg" ${RTSOPTS}


