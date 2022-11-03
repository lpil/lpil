#!/bin/sh

# You probably don't want to run this directly, instead use
# `bin/remotely-update.sh`

set -eu

PROJECT="$HOME/install"

# Install cron jobs
sudo cp -v "$PROJECT"/cron/* /etc/cron.d/
