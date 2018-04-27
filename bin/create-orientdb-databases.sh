#!/bin/sh

set -eu

USERNAME=root
PASSWORD=orientdb
AUTH=$(printf "$USERNAME:$PASSWORD" | base64)

echo Creating database particle-test

curl \
  -X POST \
  -H "Authorization: Basic $AUTH" \
  "http://localhost:2480/database/particle-test/plocal/graph"

echo
echo Creating database particle-dev

curl \
  -X POST \
  -H "Authorization: Basic $AUTH" \
  "http://localhost:2480/database/particle-dev/plocal/graph"
