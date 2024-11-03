# shellcheck shell=sh

set -eu

podman_quadlet_container() {
  local name=$1
  podman_quadlet "$name.container" "$name.service"
}

podman_quadlet_network() {
  local name=$1
  podman_quadlet "$name.network" "$name-network.service"
}

podman_quadlet() {
  local file_name=$1
  local service_name=$2
  local source="files/$file_name"
  local destination="/etc/containers/systemd/$file_name"

  echo === systemd unit $service_name ===

  # Interpolate any variables used by this source file
  cat "$source" | envsubst >"$source.tmp"
  mv "$source.tmp" "$source"

  if ! cmp --silent "$source" "$destination"; then
    echo Updating $destination
    diff --color=always "$destination" "$source" || true

    sudo install -D -m 644 -o root -g root "$source" "$destination"

    echo Reloading systemd daemon
    sudo systemctl daemon-reload

    echo Restarting $service_name
    sudo systemctl restart "$service_name"
  fi
}
