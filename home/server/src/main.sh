#!/bin/sh

# You probably don't want to run this directly, instead use
# `bin/remotely-update.sh`

set -eu

PROJECT="$HOME/install"
TAILSCALE_INSTALLED=0
SYNCTHING_INSTALLED=0
PLEX_INSTALLED=0

# Install cron jobs
echo "Installing cron jobs"
sudo cp "$PROJECT"/cron/* /etc/cron.d/

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

# Install tailscale
if ! command -v tailscale > /dev/null
then
  curl -fsSL https://tailscale.com/install.sh | sh
  sudo tailscale up
  TAILSCALE_INSTALLED=1
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

# Install plex server
if ! command -v plexmediaserver > /dev/null
then
  echo deb https://downloads.plex.tv/repo/deb public main | sudo tee /etc/apt/sources.list.d/plexmediaserver.list > /dev/null
  curl https://downloads.plex.tv/plex-keys/PlexSign.key | sudo apt-key add -
  sudo apt-get update
  sudo apt-get install plexmediaserver --yes
  PLEX_INSTALLED=1
fi

echo "Installation complete ✨"

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


if [ "$PLEX_INSTALLED" = 1 ]
then
cat << EOF

Plex installed, configure it via the web interface.
      cubone:32400/web
EOF
fi
