# shellcheck shell=sh

set -eu

. ./base.sh
. ./secrets.env

goatcounter_db=/var/lib/goatcounter/goatcounter.sqlite

install_goatcounter() {
  name=goatcounter

  download_and_install_goatcounter_binary
  service_user_and_group "$name"
  systemd_service "$name"

  wait_for_file_to_exist "$goatcounter_db"
  sudo chmod g+w "$goatcounter_db"*

  goatcounter_site hits.lpil.uk
}

goatcounter_site_exists() {
  domain=$1
  query="select site_id from sites where cname = '$domain'"
  test -n "$(sqlite3 "$goatcounter_db" "$query")"
}

goatcounter_site() {
  domain=$1

  if ! goatcounter_site_exists "$domain"
  then
    echo "Creating goatcounter site $domain"
    goatcounter db create site \
      -db sqlite+"$goatcounter_db" \
      -domain "$domain" \
      -user.email louis@lpil.uk \
      -user.password "$GOATKEEPER_LPIL_PASSWORD"
  fi
}

download_and_install_goatcounter_binary() {
  # Install goatcounter binary
  if ! command -v "$name" > /dev/null
  then
    checksum="bc1bcae8c1d686a10b3419ce778fd69fc2a78a6bb3cbb6228c34dfc3c7585920"
    file=goatcounter-v2.4.1-linux-arm64

    echo "Installing $name"
    cd /tmp
    rm -rf "$file"

    # Download
    wget --no-verbose https://github.com/arp242/goatcounter/releases/download/v2.4.1/"$file".gz

    # Decompress
    gunzip "$file".gz

    # Verify checksum
    echo "$checksum $file" | sha256sum -c -

    # Install
    install_executable "$file" "$name"

    # Clean up
    rm -r "$file" "$file".gz
    cd -
  fi
}
