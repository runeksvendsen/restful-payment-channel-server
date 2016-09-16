#!/bin/bash

function kill_all {
  pkill SigningService
  pkill PayChanServer
  pkill ChanStore
}

kill_all
set -e

echo "Starting servers..."
./runEverything.sh "$1" $4  > /dev/null 2>&1 &

echo "Sleeping 1s before starting benchmark..."
sleep 1

NUMPAYMENTS=$(echo "$2 * $3" | bc -l)
echo "Executing $NUMPAYMENTS payments..."

cd test/
time ./benchPayChanServer.sh $2 $3 localhost:8080 $4 > /dev/null

kill_all
