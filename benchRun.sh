#!/bin/bash

function kill_all {
  pkill SigningService
  pkill PayChanServer
  pkill ChanStore
}

kill_all
set -e

./runEverything.sh "$1" $4 &

echo "Sleeping 2s before starting benchmark..."
sleep 2

cd test/
time ./benchPayChanServer.sh $2 $3 localhost:8080 $4 > /dev/null


kill_all
