#!/usr/bin/sh
# Backup any existing .vimrc
if [ ~/.vimrc ]; then
  echo Backing up existing .vimrc to .vimrc.bak
  mv -iv ~/.vimrc ~/.vimrc.bak
fi
# Get full path for this directory
VIMRCPATH="$( cd "$(dirname "$0")" ; pwd -P )"

# Make the symlink
ln -sv "$VIMRCPATH/.vimrc" ~/.vimrc

# Get the plugin package manager, vundle
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle

# Install plugins
vim +BundleInstall +qall
echo "\033[0;32mDone...!\033[0m"
