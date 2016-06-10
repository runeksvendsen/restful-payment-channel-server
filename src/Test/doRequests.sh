#!/bin/bash

#curl -Ss "$RIAK/buckets/$BUCKET/keys?keys=stream" |\
#  jq -r '.keys[] | @uri' |\

jq -r '.payment_urls[]' |\
while read URL
do
  echo curl -v -X PUT -d "" "$URL"
done


