# shellcheck shell=sh

set -eu

install_gatus() {
  # Ensure Gatus group exists
  if ! getent group gatus > /dev/null
  then
    sudo groupadd --system gatus
  fi

  # Ensure Gatus user exists
  if ! getent passwd gatus > /dev/null
  then
    sudo useradd --system \
      --gid gatus \
      --create-home \
      --home-dir /var/lib/gatus \
      --shell /usr/sbin/nologin \
      --comment "Gatus health monitor" \
      gatus
  fi

  # Install gatus, a web service for monitoring other web services
  if ! command -v gatus > /dev/null
  then
    echo "Installing gatus"
    go install github.com/TwiN/gatus/v5@latest
    sudo mv ~/go/bin/gatus /usr/local/bin
  fi

  # Ensure Gatus configuration is up to date
  . ./secrets.env
  export PUSHOVER_GATUS_APPLICATION_TOKEN
  export PUSHOVER_USER_KEY
  envsubst < files/gatus.yml | sponge files/gatus.yml
  if ! cmp --silent files/gatus.yml /etc/gatus/config.yml
  then
    echo Updating Gatus config
    sudo mkdir -p /etc/gatus
    sudo cp files/gatus.yml /etc/gatus/config.yml
    sudo chown root:gatus /etc/gatus/config.yml
    sudo chmod 644 /etc/gatus/config.yml
  fi

  # Ensure Gatus systemd service is up to date
  if ! cmp --silent files/gatus.service /etc/systemd/system/gatus.service
  then
    echo Updating Gatus systemd service
    sudo mkdir -p /etc/gatus
    sudo cp files/gatus.service /etc/systemd/system/gatus.service
    sudo systemctl daemon-reload
    sudo systemctl enable gatus.service
    sudo systemctl start gatus.service
  fi
}

