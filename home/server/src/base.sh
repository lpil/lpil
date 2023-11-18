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

# Ensure systemd service is up to date
systemd_service() {
  name=$1
  if ! cmp --silent files/"$name".service /etc/systemd/system/"$name".service
  then
    echo Updating "$name" systemd service
    sudo mkdir -p /etc/"$name"
    sudo cp files/"$name".service /etc/systemd/system/"$name".service
    sudo systemctl daemon-reload
    sudo systemctl enable "$name".service
    sudo systemctl start "$name".service
  fi
}

# Create a user and group for a service
service_user_and_group() {
  name=$1

  # Ensure group exists
  if ! getent group "$name" > /dev/null
  then
    sudo groupadd --system "$name"
  fi

  # Ensure user exists
  if ! getent passwd "$name" > /dev/null
  then
    sudo useradd --system \
      --gid "$name" \
      --create-home \
      --home-dir /var/lib/"$name" \
      --shell /usr/sbin/nologin \
      --comment "$name service" \
      "$name"
  fi

  # Add you to the group
  sudo usermod -a -G "$name" louis
}

# Wait for a file to exist, or time out if it doesn't exist within a timeout
wait_for_file_to_exist() {
  file=$1
  timeout=10
  while [ ! -f "$file" ]
  do
    if [ "$timeout" -eq 0 ]
    then
      echo "Timed out waiting for $file to exist"
      exit 1
    fi
    sleep 1
    timeout=$((timeout-1))
  done
}

install_executable() {
  src=$1
  name=$2
  sudo install -D -m 755 -o root -g root "$src" /usr/local/bin/"$name"
}
