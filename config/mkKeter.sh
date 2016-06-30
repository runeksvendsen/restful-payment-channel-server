#!/bin/bash

set -e

CONF=$1
CONFDIR="$CONF/"

BINNAME="PayChanServer"
BINDIR="$(stack path --local-install-root)/bin"

FILENAME="paychan-$CONF.keter"

cd $CONFDIR
cp "$BINDIR/$BINNAME" ./
strip $BINNAME
tar czf ../$FILENAME $BINNAME config server.cfg

echo "Created $FILENAME"

