#!/bin/bash

set -e

if [ -z $3 ]; then
   echo "Usage: $0 <num_payments> <num_threads> <endpoint> <server_pubkey>"
   echo "Example: $0 1000 10 localhost:8000 0225b3aaf58992a8cc909522c2ec859ef218fd29fda0a6723cfb4e0529f80cc8f3"
   exit 1
fi

./spawnParallel.sh $1 $2 $3 $4

NUMPAYMENTS=$(echo "$1 * $2" | bc -l)
echo "Done. Executed $NUMPAYMENTS payments."
