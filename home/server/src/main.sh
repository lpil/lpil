#!/bin/sh

# You probably don't want to run this directly, instead use
# `bin/remotely-update.sh`

. "$HOME"/.profile

set -eu

PROJECT="$HOME/install"
TAILSCALE_INSTALLED=0
SYNCTHING_INSTALLED=0

# Install cron jobs
# TODO: convert to systemd timers
sudo cp "$PROJECT"/cron/* /etc/cron.d/

# Configuring default applications
sudo update-alternatives --install /usr/bin/editor editor /usr/bin/vi 100

# Update apt cache if it's older than 1 day
if [ "$(find /var/lib/apt/periodic/update-stamp -mtime +1)" ]
then
  echo "Updating apt cache"
  sudo apt-get update
fi

# Disable ssh password login
if ! grep -q "PasswordAuthentication no" /etc/ssh/sshd_config
then
  echo "Disabling ssh password login"
  echo "PasswordAuthentication no" | sudo tee -a /etc/ssh/sshd_config > /dev/null
  sudo service ssh restart
fi

# Use the unattended-upgrades package to automatically install security updates
# and other important updates.
if ! dpkg -s unattended-upgrades > /dev/null
then
  sudo apt-get update
  sudo apt-get install --yes unattended-upgrades
  cat << EOF | sudo tee /etc/apt/apt.conf.d/20auto-upgrades > /dev/null
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Unattended-Upgrade "1";
EOF
fi

# Install nfs server
if ! dpkg -s nfs-kernel-server > /dev/null
then
  echo "Installing nfs server"
  sudo apt-get install --yes nfs-kernel-server
  sudo systemctl start nfs-kernel-server.service
  echo "Rebooting server to enable nfs server"
  echo "Run this script again after rebooting"
  sudo reboot
fi
# Write the exports file
echo "/home/louis/media *(ro,all_squash,subtree_check,insecure)" | sudo tee /etc/exports > /dev/null
sudo chmod 644 /etc/exports
sudo exportfs -a
sudo systemctl reload nfs-kernel-server.service

# Install tailscale
if ! command -v tailscale > /dev/null
then
  curl -fsSL https://tailscale.com/install.sh | sh
  sudo tailscale up
  TAILSCALE_INSTALLED=1
fi

# Install Go, so that we can build Caddy with the third party CGI module
#
# NOTE: If you change the Go version:
# - Update the checksum (from https://go.dev/dl/)
if ! command -v go > /dev/null
then
  version="1.21.3"
  checksum="fc90fa48ae97ba6368eecb914343590bbb61b388089510d0c56c2dde52987ef3"
  tarball="go$version.linux-arm64.tar.gz"
  echo "Installing Go"
  cd /tmp
  rm -rf "$tarball"
  wget --no-verbose "https://go.dev/dl/$tarball"
  # Verify checksum
  echo "$checksum $tarball" | sha256sum -c -
  sudo rm -rf /usr/local/go
  sudo tar -C /usr/local -xzf "$tarball"
  rm -v "$tarball"
  cd -
fi

# Add Go bin to path if it is not there
if ! echo "$PATH" | grep -q "/usr/local/go/bin"
then
  echo "Adding Go bin to path"
  echo "export PATH=$PATH:/usr/local/go/bin" >> ~/.profile
  export PATH="$PATH:/usr/local/go/bin"
fi

# Install xcaddy, the tool for building Caddy with third party modules
if ! command -v xcaddy > /dev/null
then
  echo "Installing xcaddy"
  go install github.com/caddyserver/xcaddy/cmd/xcaddy@latest
  sudo mv ~/go/bin/xcaddy /usr/local/bin
fi

# Install Caddy
if ! command -v caddy > /dev/null
then
  xcaddy build --with github.com/aksdb/caddy-cgi/v2
  sudo mv caddy /usr/local/bin/caddy
fi

# Ensure Caddy group exists
if ! getent group caddy > /dev/null
then
  sudo groupadd --system caddy
fi

# Ensure Caddy user exists
if ! getent passwd caddy > /dev/null
then
  sudo useradd --system \
    --gid caddy \
    --create-home \
    --home-dir /var/lib/caddy \
    --shell /usr/sbin/nologin \
    --comment "Caddy web server" \
    caddy
fi

# Ensure Caddy systemd service is up to date
if ! cmp --silent "$PROJECT"/caddy.service /etc/systemd/system/caddy.service
then
  echo Updating Caddy systemd service
  sudo mkdir -p /etc/caddy
  sudo cp "$PROJECT"/caddy.service /etc/systemd/system/caddy.service
  sudo systemctl daemon-reload
  sudo systemctl enable caddy.service
  sudo systemctl start caddy.service
fi

# Ensure Caddy configuration is up to date
if ! cmp --silent "$PROJECT"/Caddyfile /etc/caddy/Caddyfile
then
  echo Updating Caddyfile
  sudo mkdir -p /etc/caddy
  sudo cp "$PROJECT"/Caddyfile /etc/caddy/Caddyfile
  sudo chown root:caddy /etc/caddy/Caddyfile
  sudo chmod 644 /etc/caddy/Caddyfile
  sudo systemctl reload caddy.service
fi

# Install syncthing
if ! command -v syncthing > /dev/null
then
  sudo curl -o /usr/share/keyrings/syncthing-archive-keyring.gpg https://syncthing.net/release-key.gpg
  echo "deb [signed-by=/usr/share/keyrings/syncthing-archive-keyring.gpg] https://apt.syncthing.net/ syncthing stable" | sudo tee /etc/apt/sources.list.d/syncthing.list > /dev/null
  sudo apt-get update
  sudo apt-get install syncthing
  sudo systemctl enable syncthing@louis.service
  SYNCTHING_INSTALLED=1
fi

# Start the syncthing systemd service if it's not running
if ! systemctl is-active --quiet syncthing@louis.service
then
  echo "Starting syncthing"
  sudo systemctl start syncthing@louis.service
fi

echo "Up to date ✨"

# Print final messages
if [ "$SYNCTHING_INSTALLED" = 1 ]
then
cat << EOF

Syncthing installed, forward its web interface to connect to it, set the
password, and start it syncing with other nodes.
      ssh -L 8000:localhost:8384 $(hostname)
EOF
fi

if [ "$TAILSCALE_INSTALLED" = 1 ]
then
cat << EOF

Tailscale installed, configure its key not to expire
https://login.tailscale.com/admin/machines
EOF
fi
