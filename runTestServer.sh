#!/bin/bash

function kill_all {
  pkill SigningService
  pkill PayChanServer
  pkill ChanStore
}

kill_all
set -e

echo "Starting servers..."
PORT=8080 ./runEverything.sh "$1" $4 &  # 2>&1 > /dev/null

echo "Sleeping 1s before starting benchmark..."
sleep 1

NUMPAYMENTS=$(echo "$2 * $3" | bc -l)
echo "Executing $NUMPAYMENTS payments using $4 core(s)..." 1>&2

cd test/
CMD="./benchPayChanServer.sh $2 $3 localhost:8080 $4"
RES=$(TIMEFORMAT='%lU';time ( ./benchPayChanServer.sh $2 $3 localhost:8080 $4 ) 2>&1 1>/dev/null)

echo "BENCHRES   $4    $RES" 1>&2

kill_all
