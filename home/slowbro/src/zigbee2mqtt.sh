# shellcheck shell=sh

set -eu

. ./helpers.sh

install_zigbee2mqtt() {
  echo === Zigbee2MQTT ===

  sudo mkdir -m 777 -p /mnt/data/zigbee2mqtt/data

  copy_file zigbee2mqtt.yml /mnt/data/zigbee2mqtt/data/configuration.yaml 444 && updated=0
  podman_quadlet_container zigbee2mqtt && updated=0

  if
    [ -n "${updated+0}" ]
  then
    systemd_restart zigbee2mqtt.service
  fi
}
