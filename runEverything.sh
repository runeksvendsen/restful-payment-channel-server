#!/bin/bash

pkill SigningService
pkill ChanStore

set -e

SigningService "$1/signing.cfg" > /dev/null &
ChanStore "$1/store.cfg" > /dev/null &

# STOREPID=$!

sleep 1
PayChanServer "$1/server.cfg" 

