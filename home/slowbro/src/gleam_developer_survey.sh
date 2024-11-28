# shellcheck shell=sh

set -eu

. ./helpers.sh

install_gleam_developer_survey() {
  echo === Gleam developer survey ===

  sudo mkdir -p /mnt/data/gleam-developer-survey
  podman_quadlet_network gleam-developer-survey || true

  if
    podman_quadlet_container gleam-developer-survey
  then
    systemd_restart gleam-developer-survey.service
  fi
}
