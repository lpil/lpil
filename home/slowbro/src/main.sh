#!/bin/sh

# You don't want to run this directly, instead use `bin/remotely-update.sh`

# shellcheck disable=SC1091
. "$HOME"/.profile

set -eu

. ./gleam_developer_survey.sh

setup_gleam_developer_survey

echo "Up to date ✨"
