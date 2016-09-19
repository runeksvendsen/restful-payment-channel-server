#!/bin/bash

if [ -z $1 ]; then
   echo "Usage: $0 <conf_prefix> <payments_per_thread> <num_threads> <max_num_cores>"
   echo "Example: $0 config/debug 1000 10 8" 
   echo "         (test 10k payments using up to 8 cores)"
   exit 1
fi

for CORES in $(seq 1 $4); do
   echo "$CORES cores"
   ./runTestServer.sh $1 $2 $3 $CORES > /dev/null
done

