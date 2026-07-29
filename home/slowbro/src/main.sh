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
. ./immich.sh
. ./syncthing.sh
. ./mosquitto.sh

install_caddy
install_cloudflare_tunnel
install_immich
install_syncthing
install_mosquitto

echo Up to date ✨
