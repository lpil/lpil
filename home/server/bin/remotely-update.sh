#!/bin/sh

set -eu

REMOTE=cubone

ssh "$REMOTE" "rm -rf install"
scp -q -o LogLevel=QUIET -r src "$REMOTE":install
ssh "$REMOTE" "sh install/main.sh"
