#!/bin/bash

set -e

if [ -z $3 ]; then
   echo "Usage: $0 <num_payments> <num_threads> <endpoint>"
   echo "Example: $0 1000 10 localhost:8000"
   exit 1
fi

./spawnParallel.sh $1 $2 $3 03b5ad36dca793edf4664cd51b4d3fb41d35d0bd92c60f62ed08b2b55fdb2c5a6d

NUMPAYMENTS=$(echo "$1 * $2" | bc -l)
echo "Done. Executed $NUMPAYMENTS payments."
