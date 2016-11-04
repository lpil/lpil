# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

export EDITOR="/usr/bin/env nvim"
export TERM=xterm

# Postgres config
export PGHOST=localhost
export PGUSER=postgres

# Path
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export PATH="$PATH:/usr/local/heroku/bin"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:$HOME/.cache/rebar3/bin"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/bin/node/bin"
export PATH="$PATH:$HOME/bin/lfe/bin"
export PATH="$PATH:$HOME/bin/go/bin"
export PATH="$HOME/bin:$PATH"

export GOPATH="$HOME/projects/go"
export GOROOT="$HOME/bin/go"
