#!/bin/sh

set -eu

REMOTE=192.168.1.77

ssh "$REMOTE" "rm -rf install"
scp -q -o LogLevel=QUIET -r src "$REMOTE":install
ssh "$REMOTE" "sh install/main.sh"
