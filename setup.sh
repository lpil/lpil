#!/usr/bin/env bash

set -e # Abort on error
set -u # Error on unset varibles
set -o pipefail

# install Vundle package manager
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# Setup undo history
mkdir -p ~/.vim/undo/

if [ "$(uname)" == "Darwin" ]; then
  cd
  infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
  tic $TERM.ti
  cd -
fi
