#!/bin/sh

set -eu

REMOTE=cubone

ssh "$REMOTE" "rm -rf install"
scp -q -o LogLevel=QUIET -r src "$REMOTE":install
scp -q -o LogLevel=QUIET secrets.env "$REMOTE":install/secrets.env
ssh "$REMOTE" "sh install/main.sh"
