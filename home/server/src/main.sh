#!/bin/sh

# You probably don't want to run this directly, instead use
# `bin/remotely-update.sh`

set -eu

PROJECT="$HOME/install"

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
