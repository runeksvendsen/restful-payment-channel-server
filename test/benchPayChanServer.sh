#!/bin/bash

set -e

./spawnParallel $1 $2 $3

NUMPAYMENTS=$(echo "$1 * $2" | bc -l)
echo "Done. Executed $NUMPAYMENTS payments."
