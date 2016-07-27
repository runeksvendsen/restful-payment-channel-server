#!/bin/bash

set -e

CONF=$1
CONFDIR="$CONF/"

BINNAME="PayChanServer"
BINNAME2="ChanStore"
BINNAME3="SigningService"
BINDIR="$(stack path --local-install-root)/bin"

FILENAME="paychan-$CONF.keter"

cd $CONFDIR
cp "$BINDIR/$BINNAME" ./
cp "$BINDIR/$BINNAME2" ./
cp "$BINDIR/$BINNAME3" ./

strip $BINNAME
strip $BINNAME2
strip $BINNAME3

tar czf ../$FILENAME $BINNAME $BINNAME2 $BINNAME3 config

echo "Created $FILENAME"

