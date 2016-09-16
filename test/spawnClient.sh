#!/bin/bash

set -e

if [ -z $3 ]; then
   echo "Usage: $0 <server_endpoint> <server_pubkey> <num_payments>"
   echo "Example: $0 \"localhost:8000\" 03b5ad36dca793edf4664cd51b4d3fb41d35d0bd92c60f62ed08b2b55fdb2c5a6d 1000"
   exit 1
fi

ENDPOINT=$1
SERVERPK=$2

TESTDATA=$(GenTestData --endpoint $ENDPOINT --pubkey $SERVERPK --testnet --pay-count $3)

SERV_URL_TMP="$(curl -v http://$ENDPOINT/funding/$SERVERPK/1502000000/begin_open | jq -r '.channel_uri')"
SERV_URL="http$SERV_URL_TMP"
DATA_URL="$(echo $TESTDATA | jq -r '.open_url')"

echo "$SERV_URL_TMP"
echo "$DATA_URL"

if [ "$SERV_URL" != "$DATA_URL" ]; then
   echo "ERROR: Test data and server disagree on payment resource URL"
   echo "Server: $SERV_OPENURL"
   echo "Data  : $DATA_OPENURL"
   exit 1
fi

echo "$TESTDATA" | RunTestData
