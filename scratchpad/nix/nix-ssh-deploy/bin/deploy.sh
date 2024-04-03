#!/bin/sh

set -eu

HOST=165.22.117.176
USER=root
ADDR=$USER@$HOST
REMOTE_NIXOS_DIR=/etc/nixos
LOCAL_SRC=./src/*

scp -r $LOCAL_SRC $ADDR:$REMOTE_NIXOS_DIR
ssh $ADDR nixos-rebuild switch
