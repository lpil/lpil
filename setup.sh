#!/usr/bin/env bash

set -e # Abort on error
set -o pipefail

# install Plug package manager
if [ ! -f ~/.vim/autoload/plug.vim ]
then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

if [ -z "${XDG_CONFIG_HOME}" ]; then
  XDG_CONFIG_HOME=$HOME/.config
fi
# Symlink neovim config into place
if [ ! -f $XDG_CONFIG_HOME/nvim/init.vim ]
then
  mkdir -p $XDG_CONFIG_HOME
  ln -s ~/.vim $XDG_CONFIG_HOME/nvim
  ln -s ~/.vimrc $XDG_CONFIG_HOME/nvim/init.vim
fi

# Setup undo history
mkdir -p ~/.vim/undo/

if [ "$(uname)" == "Darwin" ]; then
  cd
  infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
  tic $TERM.ti
  cd -
fi

echo Now run 'pip install neovim and pip3 install neovim'
