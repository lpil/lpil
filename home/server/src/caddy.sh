# shellcheck shell=sh

set -eu

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
    sudo mv ~/go/bin/xcaddy /usr/local/bin
  fi

  # Install Caddy
  if ! command -v caddy > /dev/null
  then
    xcaddy build --with github.com/aksdb/caddy-cgi/v2
    sudo mv caddy /usr/local/bin/caddy
  fi

  # Ensure Caddy group exists
  if ! getent group caddy > /dev/null
  then
    sudo groupadd --system caddy
  fi

  # Ensure Caddy user exists
  if ! getent passwd caddy > /dev/null
  then
    sudo useradd --system \
      --gid caddy \
      --create-home \
      --home-dir /var/lib/caddy \
      --shell /usr/sbin/nologin \
      --comment "Caddy web server" \
      caddy
  fi

  # Add louis to the caddy group, so that you can write to the web directories
  sudo usermod -a -G caddy louis

  make_caddy_directory "$var_data" 775
  make_caddy_directory "$web_public" 755
  make_caddy_directory "$web_private" 755
  make_caddy_directory "$web_uploads" 755
  make_caddy_directory "$cgi_bin" 755
  echo "Hello, Mike!" | sudo sponge "$web_public"/hello-joe.txt

  # Install cgi-bin scripts for use by Caddy
  sudo cp -r cgi-bin/* "$cgi_bin"/
  sudo chown -R root:root "$cgi_bin"
  sudo chmod -R 755 "$cgi_bin"
  golang_cgi_script "$cgi_bin" "file-upload"

  # Ensure Caddy systemd service is up to date
  if ! cmp --silent files/caddy.service /etc/systemd/system/caddy.service
  then
    echo Updating Caddy systemd service
    sudo mkdir -p /etc/caddy
    sudo cp files/caddy.service /etc/systemd/system/caddy.service
    sudo systemctl daemon-reload
    sudo systemctl enable caddy.service
    sudo systemctl start caddy.service
  fi

  # Ensure Caddy configuration is up to date
  if ! cmp --silent files/Caddyfile /etc/caddy/Caddyfile
  then
    echo Updating Caddyfile
    sudo mkdir -p /etc/caddy
    sudo cp files/Caddyfile /etc/caddy/Caddyfile
    sudo chown root:caddy /etc/caddy/Caddyfile
    sudo chmod 644 /etc/caddy/Caddyfile
    sudo systemctl reload caddy.service
  fi
}
