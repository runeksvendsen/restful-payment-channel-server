#!/bin/bash

pkill SigningService
pkill ChanStore

set -e

SigningService "$1/config/signing.cfg" > /dev/null &
ChanStore "$1/config/store.cfg" > /dev/null &

# STOREPID=$!

sleep 0.5
PayChanServer "$1/config/server.cfg" 

