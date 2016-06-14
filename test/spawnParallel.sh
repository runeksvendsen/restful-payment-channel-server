#!/bin/bash

set -e

if [ -z $3 ]; then
	echo "Usage: $0 <num_payments> <num_threads> <endpoint_url>"
	echo "Example: $0 1000 10 \"http://localhost:8000\""
	exit 1
fi

PIDS=""

for I in $(seq 1 "$2"); do
	echo "Spawning client $I"
	./spawnClient.sh $1 $3 &
	PID=$!
	PIDS="$PIDS $PID"	
done

echo "Waiting for clients to terminate..."
for PID in $PIDS; do
	wait $PID
done
