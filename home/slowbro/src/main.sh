#!/bin/sh

# You don't want to run this directly, instead use `bin/remotely-update.sh`

# shellcheck disable=SC1091
. "$HOME"/.profile

set -eu

# Auto export secrets for use in envsubst templates
set -a
. ./secrets.env
set +a

. ./base.sh

podman_quadlet_network caddy
podman_quadlet_container caddy
podman_quadlet_container cloudflare-tunnel
podman_quadlet_container gleam-developer-survey

echo Up to date ✨
