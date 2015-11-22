#!/usr/bin/env bash

set -e # Abort on error
set -u # Error on unset varibles
set -o pipefail

# install Plug package manager
curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Setup undo history
mkdir -p ~/.vim/undo/

if [ "$(uname)" == "Darwin" ]; then
  cd
  infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
  tic $TERM.ti
  cd -
fi
