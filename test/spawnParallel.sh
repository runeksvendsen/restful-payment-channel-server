#!/bin/bash

PIDS=""

for I in $(seq 1 "$1"); do
	echo "Spawning client $I"
	./spawnClient.sh 100 &
	PID=$!
	PIDS="$PIDS $PID"	
done

echo "Waiting for clients to terminate..."
for PID in $PIDS; do
	wait $PID
done
