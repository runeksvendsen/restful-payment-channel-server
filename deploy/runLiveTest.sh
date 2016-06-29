#!/bin/bash

screen -X -S testpay quit
screen -X -S livepay quit

screen -dmS testpay bash -c '/home/rune/.local/bin/PayChanServer -e test -p 8081'
screen -dmS livepay bash -c  '/home/rune/.local/bin/PayChanServer -e live -p 8082'
