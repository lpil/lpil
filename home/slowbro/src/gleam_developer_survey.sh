# shellcheck shell=sh

set -eu

. ./helpers.sh

install_gleam_developer_survey() {
  echo === Gleam developer survey ===

  sudo mkdir -p /mnt/data/gleam-developer-survey

  if
    podman_quadlet_container gleam-developer-survey
  then
    systemd_restart gleam-developer-survey.service
  fi
}
