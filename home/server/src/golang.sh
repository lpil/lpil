# shellcheck shell=sh

set -eu

# Compile a Go CGI script, if it hasn't already been compiled
golang_cgi_script() {
  cgi_bin="$1"
  name="$2"
  if [ ! -f "$cgi_bin"/"$name" ]
  then
    echo "Compiling cgi-bin-src/$name"
    cd /cgi-bin-src/"$name"
    go build
    sudo install -D -m 755 -o root -g root "$name" "$cgi_bin"/
  fi
}

install_golang() {
  # NOTE: If you change the Go version:
  # - Update the checksum (from https://go.dev/dl/)
  if ! command -v go > /dev/null
  then
    version="1.21.3"
    checksum="fc90fa48ae97ba6368eecb914343590bbb61b388089510d0c56c2dde52987ef3"
    tarball="go$version.linux-arm64.tar.gz"
    echo "Installing Go"
    cd /tmp
    rm -rf "$tarball"
    wget --no-verbose "https://go.dev/dl/$tarball"
    # Verify checksum
    echo "$checksum $tarball" | sha256sum -c -
    sudo rm -rf /usr/local/go
    sudo tar -C /usr/local -xzf "$tarball"
    rm -v "$tarball"
    cd -
  fi

  # Add Go bin to path if it is not there
  if ! echo "$PATH" | grep -q "/usr/local/go/bin"
  then
    echo "Adding Go bin to path"
    echo "export PATH=$PATH:/usr/local/go/bin" >> ~/.profile
    export PATH="$PATH:/usr/local/go/bin"
  fi
}
