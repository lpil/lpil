# shellcheck shell=sh

set -eu


setup_sftp() {
  # Ensure SFTP enabled
  if ! grep -qE "^Subsystem\W+sftp" /etc/ssh/sshd_config; then
    echo "Enabling SFTP"
    echo "Subsystem sftp /usr/lib/openssh/sftp-server" | sudo tee --append /etc/ssh/sshd_config
  fi

  . ./secrets.env
  sftp_guest sftp-machop "$SFTP_MACHOP_PASSWORD"
}

sftp_guest() {
  name=$1
  pass=$2

  create_group "$name"
  create_nologin_user "$name"
  echo "$name:$pass" | sudo chpasswd

  # Add you to the group
  sudo usermod -a -G "$name" louis

  # Ensure SSH config is up to date
  src=files/sftp_user_ssh_"$name".conf
  dest=/etc/ssh/sshd_config.d/"$name".conf
  export SFTP_USER_NAME="$name"
  envsubst < files/sftp_user_ssh.conf | sponge "$src"
  if ! cmp --silent "$src" "$dest"
  then
    echo Updating "$name" ssh config
    sudo install -D -m 644 -o root -g root "$src" "$dest"
    sudo systemctl restart ssh
  fi
}
