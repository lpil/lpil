# shellcheck shell=sh

set -eu

. ./base.sh

install_gatus() {
  service_user_and_group gatus

  # Install gatus, a web service for monitoring other web services
  if ! command -v gatus > /dev/null
  then
    echo "Installing gatus"
    go install github.com/TwiN/gatus/v5@latest
    install_executable ~/go/bin/gatus gatus
  fi

  # Ensure Gatus configuration is up to date
  . ./secrets.env
  export PUSHOVER_GATUS_APPLICATION_TOKEN
  export PUSHOVER_USER_KEY
  envsubst < files/gatus.yml | sponge files/gatus.yml
  if ! cmp --silent files/gatus.yml /etc/gatus/config.yml
  then
    echo Updating Gatus config
    sudo install -D -m 644 -o root -g gatus files/gatus.yml /etc/gatus/config.yml
  fi

  # Ensure Gatus systemd service is up to date
  systemd_service gatus
}

