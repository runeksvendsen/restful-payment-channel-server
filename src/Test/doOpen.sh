#!/bin/bash

set -e

INPUT=`cat`
OPENURL=$(echo "$INPUT" | jq -r '.open_url')

curl -v -X POST -d "" "$OPENURL"

echo "$INPUT"

