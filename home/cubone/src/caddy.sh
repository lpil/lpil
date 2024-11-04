# shellcheck shell=sh

set -eu

. ./base.sh
. ./golang.sh

cgi_bin=/usr/lib/cgi-bin
var_data=/var/lib/lpil
web_public=/srv/web-public
web_private=/srv/web-private
web_uploads=/srv/web-uploads

make_caddy_directory() {
  dir="$1"
  permissions="$2"
  sudo mkdir -p "$dir"
  sudo chown -R root:caddy "$dir"
  sudo chmod -R "$permissions" "$dir"
}

install_caddy() {
  # Install xcaddy, the tool for building Caddy with third party modules
  if ! command -v xcaddy > /dev/null
  then
    echo "Installing xcaddy"
    go install github.com/caddyserver/xcaddy/cmd/xcaddy@latest
    install_executable ~/go/bin/xcaddy xcaddy
  fi

  # Install Caddy
  if ! command -v caddy > /dev/null
  then
    xcaddy build --with github.com/aksdb/caddy-cgi/v2
    install_executable caddy caddy
  fi

  service_user_and_group caddy

  # Add louis to the caddy group, so that you can write to the web directories
  sudo usermod -a -G caddy louis

  make_caddy_directory "$var_data" 775
  make_caddy_directory "$web_public" 775
  make_caddy_directory "$web_private" 775
  make_caddy_directory "$web_uploads" 775
  make_caddy_directory "$cgi_bin" 755
  echo "Hello, Mike!" | sudo sponge "$web_public"/hello-joe.txt

  # Install cgi-bin scripts for use by Caddy
  golang_cgi_script "$cgi_bin" "file-upload"

  # Ensure Caddy systemd service is up to date
  systemd_service caddy

  # Ensure Caddy configuration is up to date
  if ! cmp --silent files/Caddyfile /etc/caddy/Caddyfile
  then
    echo Updating Caddyfile
    sudo install -D -m 644 -o root -g caddy files/Caddyfile /etc/caddy/Caddyfile
    sudo systemctl reload caddy.service
  fi
}
