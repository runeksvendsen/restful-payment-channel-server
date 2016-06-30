#!/bin/bash

set -e

CFGFILE="$1"
BIN="PayChanServer"
DEPLOYDIR="/opt/keter/incoming/"

# arg1: "deploy" or something else
# arg2: filename
function maybeDeploy {
   if [ "$1" == "deploy" ]; then
      cp "$2" "$DEPLOYDIR"
   fi
}

stack build
cp .stack-work/install/x86_64-linux/lts-5.2/7.10.3/bin/$BIN ./
strip $BIN

FILENAME="paychan-$1.keter"
tar czfv $FILENAME $BIN dist config $CFGFILE

echo "Created $FILENAME"

# maybeDeploy $1 $FILENAME

