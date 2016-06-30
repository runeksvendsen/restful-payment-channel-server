#!/bin/bash

CWD=$(pwd)

cd ..
stack build

cd "$CWD"
./mkKeter.sh live
./mkKeter.sh test
