# shellcheck shell=sh

set -eu

. ./helpers.sh

install_zigbee2mqtt() {
  echo === Zigbee2MQTT ===

  unset updated

  sudo mkdir -m 755 -p /mnt/data/zigbee2mqtt/data/external_extensions
  sudo mkdir -m 755 -p /mnt/data/sync-data/zigbee-record

  copy_template zigbee2mqtt.yml /mnt/data/zigbee2mqtt/data/configuration.yaml 444 && updated=0
  copy_file zigbee2mqtt-extension.mjs /mnt/data/zigbee2mqtt/data/external_extensions/zigbee2mqtt-extension.mjs 444 && updated=0
  podman_quadlet_container zigbee2mqtt && updated=0

  if
    [ -n "${updated+0}" ]
  then
    systemd_restart zigbee2mqtt.service
  fi
}
