#!/bin/bash

set -e

if [ -z $2 ]; then
	echo "Usage: $0 <num_payments> <endpoint_url>"
	echo "Example: $0 1000 \"http://localhost:8000\""
	exit 1
fi

GenTestData --endpoint "$2" --testnet --pay-count $1 | RunTestData
