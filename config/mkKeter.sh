#!/bin/bash

set -e

CONF=$1
CONFDIR="$CONF/"

BINNAME="PayChanServer"
BINNAME2="ChanStore"
BINDIR="$(stack path --local-install-root)/bin"

FILENAME="paychan-$CONF.keter"

cd $CONFDIR
cp "$BINDIR/$BINNAME" ./
cp "$BINDIR/$BINNAME2" ./
strip $BINNAME
strip $BINNAME2
tar czf ../$FILENAME $BINNAME $BINNAME2 config

echo "Created $FILENAME"

