#!/bin/bash

TESTDATA="data/test.json"

set -e

OPENURL=$(jq -r '.open_url' < "$TESTDATA")

curl -v -X POST -d "" "$OPENURL"


jq -r '.payment_urls[]' < "$TESTDATA"  |\
while read URL
do
  curl -v -X PUT -d "" "$URL" 
done
