# shellcheck shell=sh

set -eu

install_syncthing() {
  if ! command -v syncthing > /dev/null
  then
    sudo curl -o /usr/share/keyrings/syncthing-archive-keyring.gpg https://syncthing.net/release-key.gpg
    echo "deb [signed-by=/usr/share/keyrings/syncthing-archive-keyring.gpg] https://apt.syncthing.net/ syncthing stable" | sudo tee /etc/apt/sources.list.d/syncthing.list > /dev/null
    sudo apt-get update
    sudo apt-get install syncthing
    sudo systemctl enable syncthing@louis.service
    echo 1 # Inform caller that syncthing was installed
  fi

  # Start the syncthing systemd service if it's not running
  if ! systemctl is-active --quiet syncthing@louis.service
  then
    echo "Starting syncthing"
    sudo systemctl start syncthing@louis.service
  fi
}

synthing_post_install() {
  if [ "$1" = 1 ]
  then
  cat << EOF

Syncthing installed, forward its web interface to connect to it, set the
password, and start it syncing with other nodes.
      ssh -L 8000:localhost:8384 $(hostname)
EOF
  fi
}
