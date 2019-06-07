#!/bin/sh
set -eu

aws lambda invoke \
  --function-name=blondie_poller \
  --invocation-type=RequestResponse \
  --payload='{ "test": "value" }' \
  --log-type=Tail \
  /dev/null | jq -r '.LogResult' | base64 --decode
