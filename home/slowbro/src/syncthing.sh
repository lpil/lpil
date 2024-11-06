# shellcheck shell=sh

set -eu

. ./helpers.sh

install_syncthing() {
  echo === Syncthing ===

  sudo mkdir -m 777 -p /mnt/data/syncthing
  sudo mkdir -m 777 -p /mnt/data/sync-data

  if
    podman_quadlet_container syncthing
  then
    systemd_restart syncthing.service
  fi
}
