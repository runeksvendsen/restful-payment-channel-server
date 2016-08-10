#!/bin/bash

set -e

if [ -z $3 ]; then
   echo "Usage: $0 <num_payments> <num_threads> <endpoint>"
   echo "Example: $0 1000 10 localhost:8000"
   exit 1
fi

ENDPOINT="$3"
PUBKEY=$(curl --silent "http://$ENDPOINT/v1/fundingInfo?client_pubkey=03da3afe4f58992a8cc909522c2ec859ef218fd92fda0a67c23fb40e0303030405&exp_time=1500000000" | jq -r ".server_pubkey")

./spawnParallel.sh $1 $2 $ENDPOINT $PUBKEY

NUMPAYMENTS=$(echo "$1 * $2" | bc -l)
echo "Done. Executed $NUMPAYMENTS payments."
