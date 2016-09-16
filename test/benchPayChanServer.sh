#!/bin/bash

set -e

if [ -z $3 ]; then
   echo "Usage: $0 <num_payments> <num_threads> <endpoint>"
   echo "Example: $0 1000 10 localhost:8000"
   exit 1
fi

ENDPOINT="$3"

## Get server pubkey
CLIENTPK=03da3afe4f58992a8cc909522c2ec859ef218fd92fda0a67c23fb40e0303030405
EXP=1500000000
PUBKEY=$(curl --silent "http://$ENDPOINT/funding/$CLIENTPK/$EXP/info" | jq -r ".server_pubkey")
if [ -z $PUBKEY ]; then
   echo "Failed to get pubkey from $ENDPOINT"
   exit 1
fi

SERV_OPENURL=$(curl --silent "http://$ENDPOINT/funding/$CLIENTPK/$EXP/begin_open" | jq -r ".channel_uri")

# 1000 10 localhost 023343434
./spawnParallel.sh $1 $2 $ENDPOINT $PUBKEY

NUMPAYMENTS=$(echo "$1 * $2" | bc -l)
echo "Done. Executed $NUMPAYMENTS payments."
