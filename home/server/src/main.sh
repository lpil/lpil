#!/bin/sh

# You probably don't want to run this directly, instead use
# `bin/remotely-update.sh`

set -eu

PROJECT="$HOME/install"
TAILSCALE_INSTALLED=0

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

# Install tailscale
if ! command -v tailscale > /dev/null
then
  curl -fsSL https://tailscale.com/install.sh | sh
  sudo tailscale up
  TAILSCALE_INSTALLED=1
fi

echo "Installation complete ✨"

# Print final messages
[ "$TAILSCALE_INSTALLED" = 1 ] && cat << EOF

Tailscale installed, configure its key not to expire
https://login.tailscale.com/admin/machines
EOF
