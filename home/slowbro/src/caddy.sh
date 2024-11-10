# shellcheck shell=sh

set -eu

. ./helpers.sh

install_caddy() {
  echo === Caddy reverse proxy ===

  sudo mkdir -p /mnt/data/caddy/data
  sudo mkdir -p /mnt/data/caddy/config

  podman_quadlet_network caddy || true

  copy_file Caddyfile /mnt/data/caddy/Caddyfile 444 && updated=0
  podman_quadlet_container caddy && updated=0

  if
    [ -n "${updated+0}" ]
  then
    systemd_restart caddy.service
  fi
}
