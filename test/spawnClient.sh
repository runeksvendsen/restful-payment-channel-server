#!/bin/bash

GenTestData --endpoint "http://localhost:8000" --testnet --pay-count $1 | RunTestData
