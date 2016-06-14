#!/bin/bash

TESTDATA="data/test.json"

set -e

OPENURL=$(jq -r '.open_url' < "$TESTDATA")

curl --silent -X POST -d "" "$OPENURL"


jq -r '.payment_urls[]' < "$TESTDATA"  |\
while read URL
do
  curl --silent -X PUT -d "" "$URL" 
done
