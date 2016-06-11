#!/bin/bash

set -e

OPENURL=$(jq -r '.open_url')

curl --silent -X POST -d "" "$OPENURL"

