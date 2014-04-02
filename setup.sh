#!/bin/sh
# Backup any existing .vimrc
if [ ~/.vimrc ]; then
  echo Backing up existing .vimrc to .vimrc.bak
  mv -iv ~/.vimrc ~/.vimrc.bak
fi
# Backup any existing .vimrc
if [ ~/.vimperatorrc ]; then
  echo
  echo Backing up existing .vimperatorrc to .vimperatorrc.bak
  mv -iv ~/.vimperatorrc ~/.vimperatorrc.bak
fi

# Get full path for this directory
VIMRCPATH="$( cd "$(dirname "$0")" ; pwd -P )"

# Make the symlinks
echo
echo Creating Symlinks
ln -sv "$VIMRCPATH/.vimrc" ~/.vimrc
ln -sv "$VIMRCPATH/.vimperatorrc" ~/.vimperatorrc

# Make the undo history dir. Vim ought to do this.
mkdir -p ~/.vim/undo

# Get the plugin package manager, vundle
if ! [ ~/.vim/bundle/vundle ]; then
  echo
  echo Installing vundle, the vim package manager
  git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
else
  echo
  echo Existing vundle install detected
fi

# Install plugins
echo
echo Installing plugins via vundle
vim +BundleInstall +qall
echo "\033[0;32mDone...!\033[0m"
