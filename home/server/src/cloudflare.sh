# shellcheck shell=sh

set -eu

. ./base.sh

install_cloudflared() {
  # sudo mkdir -p /etc/cloudflared

  # Add Cloudflare's keyring file if not present
  if ! [ -f /usr/share/keyrings/cloudflare-main.gpg ]
  then
    curl -fsSL https://pkg.cloudflare.com/cloudflare-main.gpg | sudo tee /usr/share/keyrings/cloudflare-main.gpg > /dev/null
  fi

  # Add the cloudflared repository if not present
  if ! grep -q cloudflare /etc/apt/sources.list /etc/apt/sources.list.d/*
  then
    echo "deb [signed-by=/usr/share/keyrings/cloudflare-main.gpg] https://pkg.cloudflare.com/cloudflared $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/cloudflared.list > /dev/null
    sudo apt-get update
  fi

  install_apt_package cloudflared

  # Ensure Cloudflared group exists
  if ! getent group cloudflared > /dev/null
  then
    sudo groupadd --system cloudflared
  fi
  sudo usermod -a -G cloudflared louis

  # Ensure Cloudflared user exists
  if ! getent passwd cloudflared > /dev/null
  then
    sudo useradd --system \
      --gid cloudflared \
      --create-home \
      --home-dir /var/lib/cloudflared \
      --shell /usr/sbin/nologin \
      --comment "Cloudflared tunnel daemon" \
      cloudflared
  fi

  # Ensure Cloudflared systemd service is up to date
  . ./secrets.env
  export CLOUDFLARED_TOKEN
  envsubst < files/cloudflared.service | sponge files/cloudflared.service
  if ! cmp --silent files/cloudflared.service /etc/systemd/system/cloudflared.service
  then
    echo Updating cloudflared systemd service
    sudo mkdir -p /etc/cloudflared
    sudo cp files/cloudflared.service /etc/systemd/system/cloudflared.service
    sudo systemctl daemon-reload
    sudo systemctl enable cloudflared.service
    sudo systemctl restart cloudflared.service
  fi
}
