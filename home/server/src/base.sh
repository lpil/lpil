# shellcheck shell=sh

set -eu

install_apt_package() {
  if ! dpkg -s "$1" > /dev/null
  then
    echo "Installing $1"
    sudo apt-get install --yes "$1"
  fi
}

disable_ssh_password_login() {
  if ! grep -q "PasswordAuthentication no" /etc/ssh/sshd_config
  then
    echo "Disabling ssh password login"
    echo "PasswordAuthentication no" | sudo tee -a /etc/ssh/sshd_config > /dev/null
    sudo service ssh restart
  fi
}

enable_unattended_upgrades() {
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
}

ensure_apt_cache_fresh() {
  # Update apt cache if it's older than 1 day
  if [ "$(find /var/lib/apt/periodic/update-stamp -mtime +1)" ]
  then
    echo "Updating apt cache"
    sudo apt-get update
  fi
}

install_cronjobs() {
  # TODO: convert to systemd timers
  sudo cp cron/* /etc/cron.d/
  sudo chown -R root:root /etc/cron.d
  sudo chmod -R 644 /etc/cron.d
}

