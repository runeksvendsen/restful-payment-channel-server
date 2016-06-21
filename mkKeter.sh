#!/bin/bash

BIN="PayChanServer"

stack build
cp .stack-work/install/x86_64-linux/lts-5.2/7.10.3/bin/$BIN ./
strip $BIN
tar czfv paychan.keter $BIN dist devel.cfg

