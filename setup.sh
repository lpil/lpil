#!/usr/bin/sh
# Backup any existing .vimrc
if [ ~/.vimrc ]; then
  echo Backing up existing .vimrc to .vimrc.bak
  mv -iv ~/.vimrc ~/.vimrc.bak
fi
ln -sv $(dirname $0)/.vimrc ~/.vimrc
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
vim +BundleInstall +qall
echo Done...!
