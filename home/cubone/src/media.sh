# shellcheck shell=sh

set -eu

setup_nfs() {
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
  echo "/home/louis/media *(ro,all_squash,subtree_check,insecure)" | sudo sponge /etc/exports
  sudo chmod 644 /etc/exports
  sudo exportfs -a
  sudo systemctl reload nfs-kernel-server.service
}
