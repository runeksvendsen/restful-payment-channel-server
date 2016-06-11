#!/bin/bash

set -e

#INPUT=`cat`
#OPENURL=$(echo "$INPUT" | jq -r '.open_url')

OPENURL=$(jq -r '.open_url')

curl --silent -X POST -d "" "$OPENURL"

#echo "$INPUT"

