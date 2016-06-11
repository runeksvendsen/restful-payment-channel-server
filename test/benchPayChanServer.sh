#!/bin/bash

if [ -z "$2" ]; then
	echo "Usage: $0 <num_payments_per_thread> <num_threads>"
	echo "Example: "
	echo "$0 10000 32"
	exit 1
fi

PIDS=""
for I in $(seq 1 $2); do
	GenTestData --endpoint "http://192.168.1.35:8000" --testnet --pay-count $1 | RunTestData &
	PIDS="$PIDS $!"
done

for PID in $PIDS; do
	wait $PID
done

