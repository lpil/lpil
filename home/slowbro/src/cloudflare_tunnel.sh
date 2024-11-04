# shellcheck shell=sh

set -eu

. ./helpers.sh

install_cloudflare_tunnel() {
  echo === Cloudflare tunnel ===

  if
    podman_quadlet_container cloudflare-tunnel
  then
    systemd_restart cloudflare-tunnel.service
  fi
}
