#!/bin/sh

# You don't want to run this directly, instead use `bin/remotely-update.sh`

# shellcheck disable=SC1091
. "$HOME"/.profile

set -eu

. ./base.sh
. ./caddy.sh
. ./gatus.sh
. ./media.sh
. ./golang.sh
. ./syncthing.sh
. ./tailscale.sh
. ./cloudflare.sh
. ./goatcounter.sh

sudo update-alternatives --install /usr/bin/editor editor /usr/bin/vi 100

ensure_apt_cache_fresh
install_apt_package sqlite3

disable_ssh_password_login
enable_unattended_upgrades
install_cronjobs
tailscale_installed=$(install_tailscale)

setup_nfs
install_golang
install_goatcounter
install_gatus
install_caddy
install_cloudflared
syncthing_installed=$(install_syncthing)

echo "Up to date ✨"

synthing_post_install "$syncthing_installed"
tailscale_post_install "$tailscale_installed"
