# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ -f ~/.bash_aliases.sh ]; then
    . ~/.bash_aliases.sh
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

###########
#  Prompt #
###########

# Local users have a green prompt
# Remote users have a yellow prompt
# `root` has a red prompt
# If we are connected remotely, `@<hostname>` shows first.

# Displays git repo status info
source ~/.git_prompt.sh
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWDIRTYSTATE=1

build_ps1() {
  # Green default prompt
  local color='\[\e[1;32m\]'
  local host=''

  # If ssh, yellow prompt, include host name
  [[ $SSH_TTY ]] && color='\[\e[1;33m\]'
  [[ $SSH_TTY ]] && host="@$HOSTNAME"

  # If root, red prompt
  [[ $UID -eq 0 ]] && color='\[\e[1;31m\]'

  # Build
  local part1="${color}\u${host} "
  local part2="\[\e[1;34m\]\w${color}"
  local part3='$(__git_ps1 " [%s]") \[\e[m\]\$'
  echo $part1$part2$part3' '
}
PS1=$(build_ps1)


# Ruby rbenv version manager
if [ -d "$HOME/.rbenv" ]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi
# Node nodenv version manager
if [ -d "$HOME/.nodenv" ]; then
  export PATH="$HOME/.nodenv/bin:$PATH"
  eval "$(nodenv init -)"
fi

# homeshick config manager
if [ -d "$HOME/.homesick/repos/homeshick/bin" ]; then
  export PATH="$HOME/.homesick/repos/homeshick/bin:$PATH"
fi


# Start autojump on Debian
if [ -f "/usr/share/autojump/autojump.sh" ]; then
  source /usr/share/autojump/autojump.sh
fi
# Start autojump on OSX
[[ $(type -P "brew") ]] && if [ -f $(brew --prefix)/etc/profile.d/autojump.sh ]; then
  source $(brew --prefix)/etc/profile.d/autojump.sh
fi

# OSX bash completion
[[ $(type -P "brew") ]] && if [ -f $(brew --prefix)/etc/bash_completion ]; then
  source $(brew --prefix)/etc/bash_completion
fi
