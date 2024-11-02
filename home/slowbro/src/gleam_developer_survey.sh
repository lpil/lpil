# shellcheck shell=sh

set -eu

setup_gleam_developer_survey() {
  mkdir -p /mnt/data/gleam-developer-survey

  if ! cmp --silent files/gleam_developer_survey.container /etc/containers/systemd/gleam-developer-survey.container; then
    echo Updating gleam-developer-survey.container
    sudo install -D -m 644 -o root -g root files/gleam_developer_survey.container /etc/containers/systemd/gleam-developer-survey.container

    echo Reloading systemd daemon
    sudo systemctl daemon-reload

    echo Restarting gleam-developer-survey.service
    sudo systemctl restart gleam-developer-survey.service
  fi
}
