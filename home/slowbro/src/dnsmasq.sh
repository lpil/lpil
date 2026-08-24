# shellcheck shell=sh

set -eu

. ./helpers.sh

install_dnsmasq() {
  echo === dnsmasq DNS server ===

  sudo mkdir -p /mnt/data/dnsmasq/

  copy_file dnsmasq.conf /mnt/data/dnsmasq/dnsmasq.conf 444 && updated=0
  podman_quadlet_container dnsmasq && updated=0

  if
    [ -n "${updated+0}" ]
  then
    systemd_restart dnsmasq.service
  fi
}
