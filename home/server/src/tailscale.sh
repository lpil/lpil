# shellcheck shell=sh

set -eu

install_tailscale() {
  if ! command -v tailscale > /dev/null
  then
    curl -fsSL https://tailscale.com/install.sh | sh
    sudo tailscale up
    echo 1 # Inform caller that tailscale was installed
  fi

  # Expose web server to the internet using tailscale
  sudo tailscale serve https / http://localhost:80
  sudo tailscale funnel 443 on
}

tailscale_post_install() {
  if [ "$1" = 1 ]
  then
  cat << EOF

Tailscale installed, configure its key not to expire
https://login.tailscale.com/admin/machines
EOF
  fi
}
