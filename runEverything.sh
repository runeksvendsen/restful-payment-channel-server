#!/bin/bash

OPT=""

pkill SigningService
pkill ChanStore

set -e

SigningService "$1/config/signing.cfg" > /dev/null &
sleep 0.1
ChanStore "$1/config/store.cfg" "$OPT" > /dev/null &

sleep 0.3
PayChanServer "$1/config/server.cfg" "$OPT"


