#!/bin/sh

# You don't want to run this directly, instead use `bin/remotely-update.sh`

# shellcheck disable=SC1091
. "$HOME"/.profile

set -eu

# Auto export secrets for use in envsubst templates
set -a
. ./secrets.env
set +a

. ./helpers.sh
. ./caddy.sh
. ./cloudflare_tunnel.sh
. ./gleam_developer_survey.sh
. ./syncthing.sh

install_caddy
install_cloudflare_tunnel
install_gleam_developer_survey
install_syncthing

echo Up to date ✨
