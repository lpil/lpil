# shellcheck shell=sh

set -eu

. ./helpers.sh

install_immich() {
  echo === Immich photo app ===

  sudo mkdir -p /mnt/data/immich/postgresql
  sudo mkdir -p /mnt/data/immich/config
  sudo mkdir -p /mnt/data/immich/photos

  # Syncthing backed up directories
  sudo mkdir -p /mnt/data/sync-data/immich/library
  sudo mkdir -p /mnt/data/sync-data/immich/upload
  sudo mkdir -p /mnt/data/sync-data/immich/profile

  podman_quadlet_network immich || true

  if
    podman_quadlet_container immich-valkey
  then
    systemd_restart immich-valkey.service
  fi

  if
    podman_quadlet_container immich-postgresql
  then
    systemd_restart immich-postgresql.service
  fi

  if
    podman_quadlet_container immich
  then
    systemd_restart immich.service
  fi
}
