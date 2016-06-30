#!/bin/bash

stack build
./mkKeter.sh live
./mkKeter.sh test
