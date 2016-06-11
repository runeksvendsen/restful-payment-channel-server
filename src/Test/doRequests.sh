#!/bin/bash

set -e

jq -r '.payment_urls[]' |\
while read URL
do
  curl --silent -X PUT -d "" "$URL" > /dev/null
done


