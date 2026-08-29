# shellcheck shell=sh

set -eu

. ./helpers.sh

install_mpd() {
  echo === mpd music player ===

  unset updated

  sudo mkdir -p /mnt/data/mpd/data
  sudo chmod 777 /mnt/data/mpd/data

  copy_file mpd.conf /mnt/data/mpd/mpd.conf 444 && updated=0
  podman_quadlet_container mpd && updated=0

  if
    [ -n "${updated+0}" ]
  then
    systemd_restart mpd.service
  fi
}
