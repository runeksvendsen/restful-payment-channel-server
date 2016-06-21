#!/bin/bash

set -e

CFGFILE="paychan.cfg"
BIN="PayChanServer"
DEPLOYDIR="/opt/keter/incoming/"

function mkNetConfString {
   echo "bitcoin { network = \"$1\" }"
}

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

for NET in $(echo "live test"); do
   cp $CFGFILE.nonet $CFGFILE
   CONFSTR="$(mkNetConfString $NET)"
   echo "$CONFSTR" >> $CFGFILE
   FILENAME="paychan-$NET.keter"
   tar czfv $FILENAME $BIN dist config $CFGFILE
   maybeDeploy $1 $FILENAME
done

