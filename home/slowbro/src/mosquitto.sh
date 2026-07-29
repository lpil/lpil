# shellcheck shell=sh

set -eu

. ./helpers.sh

install_mosquitto() {
  echo === Mosquitto ===

  sudo mkdir -m 777 -p /mnt/data/mosquitto/config

  copy_file mosquitto.conf /mnt/data/mosquitto/config/mosquitto.conf 444 && updated=0
  copy_file mosquitto_passwords /mnt/data/mosquitto/config/passwords 444 && updated=0
  podman_quadlet_container mosquitto && updated=0

  if
    [ -n "${updated+0}" ]
  then
    systemd_restart mosquitto.service
  fi
}
