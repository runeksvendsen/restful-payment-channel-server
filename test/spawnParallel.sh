#!/bin/bash

set -e

if [ -z $3 ]; then
	echo "Usage: $0 <num_payments> <num_threads> <endpoint> <server_pubkey>"
	echo "Example: $0 1000 10 \"localhost:8000\"" 03b5ad36dca793edf4664cd51b4d3fb41d35d0bd92c60f62ed08b2b55fdb2c5a6d
	exit 1
fi

PIDS=""

for I in $(seq 1 "$2"); do
	echo "Spawning client $I"
	./spawnClient.sh $3 $4 $1 &
	PID=$!
	PIDS="$PIDS $PID"	
done

echo "Waiting for clients to terminate..."
for PID in $PIDS; do
	wait $PID
done
