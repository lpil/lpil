#!/bin/sh
# shellcheck disable=SC2029

set -eu

remote=cubone
remote_dir=/tmp/server-install

ssh "$remote" "rm -rf $remote_dir"
scp -q -o LogLevel=QUIET -r src "$remote:$remote_dir"
ssh "$remote" "cd $remote_dir && sh main.sh && rm -rf $remote_dir"
