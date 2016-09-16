#!/bin/bash

set -e

if [ -z $3 ]; then
   echo "Usage: $0 <server_endpoint> <server_pubkey> <num_payments> <client_pubkey>"
   echo "Example: $0 \"localhost:8000\" <hex-encoded pubkey> 1000 <hex-encoded pubkey>"
   exit 1
fi

ENDPOINT=$1
SERVERPK=$2
CLIENTPK=$4

TESTDATA=$(GenTestData --endpoint $ENDPOINT --pubkey $SERVERPK --testnet --pay-count $3)

#SERV_URL="http$(curl -v http://$ENDPOINT/funding/$CLIENTPK/1502000000/begin_open | jq -r '.channel_uri')"
#DATA_URL="$(echo $TESTDATA | jq -r '.open_url')"

#if [ "$SERV_URL" != "$DATA_URL" ]; then
#   echo "ERROR: Test data and server disagree on payment resource URL"
#   echo "Server: $SERV_OPENURL"
#   echo "Data  : $DATA_OPENURL"
#   exit 1
#else
#   echo "SUCCESS: begin_open reports correct URI"
#fi

#echo "Test data:"
#echo "$TESTDATA"

echo "$TESTDATA" | RunTestData
