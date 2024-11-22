# shellcheck shell=sh

set -eu

podman_quadlet_container() {
  local name=$1
  podman_quadlet "$name.container"
  return $?
}

podman_quadlet_network() {
  local name="$1"
  if
    podman_quadlet "$name.network"
  then
    sudo systemctl start "$name-network"
    return 0
  else
    return 1
  fi
}

podman_quadlet() {
  local name=$1

  if
    copy_file "$name" "/etc/containers/systemd/$name"
  then
    systemd_reload
    return 0
  else
    return 1
  fi
}

systemd_service() {
  local name=$1

  if
    copy_file "$name" "/etc/systemd/system/$name"
  then
    systemd_reload
    return 0
  else
    return 1
  fi
}

systemd_reload() {
  echo Reloading systemd daemon
  sudo systemctl daemon-reload
}

systemd_restart() {
  local name="$1"
  echo Restarting $name
  sudo systemctl restart "$name"
}

copy_file() {
  local file_name="$1"
  local destination="$2"
  local permission=${3:-644}
  local source="files/$file_name"
  local tmp="$source.tmp"

  # Interpolate any variables used by this source file
  cat "$source" | envsubst >"$tmp"

  if ! cmp --silent "$tmp" "$destination"; then
    echo Updating $destination
    show_diff "$tmp" "$destination"
    sudo install -D -m "$permission" -o root -g root "$tmp" "$destination"
    return 0
  else
    rm "$tmp"
    return 1
  fi

}

show_diff() {
  local source="$1"
  local destination="$2"
  diff --color=always "$destination" "$source" || true
}
