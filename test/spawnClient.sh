#!/bin/bash

set -e

if [ -z $2 ]; then
	echo "Usage: $0 <server_endpoint_url> <server_pubkey> <num_payments>"
	echo "Example: $0 03b5ad36dca793edf4664cd51b4d3fb41d35d0bd92c60f62ed08b2b55fdb2c5a6d 1000 \"http://localhost:8000\""
	exit 1
fi

GenTestData --endpoint "$1" --pubkey "$2" --testnet --pay-count $3 | RunTestData
