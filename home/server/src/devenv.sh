# shellcheck shell=sh

set -eu

install_devenv() {
	if ! command -v nvim >/dev/null; then
		echo "Installing Neovim"

		url="https://github.com/matsuu/neovim-aarch64-appimage/releases/download/v0.9.4/nvim-v0.9.4-aarch64.appimage"
		checksum="9c8a9ac2eb8dc134c9a6f892afbe51abed9e742414908e48f7a630b23ae994ee"
		cd /tmp
		rm -rf nvim
		wget --no-verbose "$url" -O nvim
		# Verify checksum
		echo "$checksum nvim" | sha256sum -c -
		chmod +x nvim
		sudo rm -rf /usr/local/bin/nvim
		sudo mv nvim /usr/local/bin/nvim
		cd -
	fi
}
