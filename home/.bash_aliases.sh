# Hub, yo
command -v hub >/dev/null 2>&1 && {
  alias git=hub
}

# apt
alias agupdate='sudo apt-get update && sudo apt-get dist-upgrade'

# Quick dir sharing between terms
alias pd='echo $PWD > ~/.lastdir'
alias gd='cd "$(cat ~/.lastdir)"'

# OSX
if [ $(uname) == Darwin ]; then
  alias ls='ls -G'
  alias tree='tree -C'
  alias less='less -R'
  alias tulpn='sudo lsof -nP -iTCP -sTCP:LISTEN'

  # Launch sublime in cwd
  alias subl="open -a 'sublime text' ."
fi

source "$HOME/.aliases.sh"
