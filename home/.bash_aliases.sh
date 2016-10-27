# Ring the bell
alias bell='echo -e \\a'

# Internet up?
alias up='time ping 4.2.2.2'

# startx
alias x='startx'

# vim
alias v='vim'
alias n='nvim'

# apt
alias agupdate='sudo apt-get update && sudo apt-get dist-upgrade'

# ls
alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias lla='ls -lah'

# i3lock black background
alias i3lock='i3lock -c 303030'

# cd
alias c='cd'

# Quick dir sharing between terms
alias pd='echo $PWD > ~/.lastdir'
alias gd='cd "$(cat ~/.lastdir)"'

# Up dirs
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Go to ~/downloads
alias dl='cd ~/downloads'

# Hub, yo
command -v hub >/dev/null 2>&1 && {
  alias git=hub
}

#################
# Default flags #
#################

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'

  alias grep='grep -i --color=auto'
  alias fgrep='fgrep -i --color=auto'
  alias egrep='egrep -i --color=auto'
  alias tree='tree -C'

  alias less='less -R'
fi

# Case insensitive locate
alias locate='locate -i'

# feh images fit window size
alias feh='feh -.'

# ruby: bundle exec shorthands
alias be='bundle exec'
alias ber='bundle exec rake'

# pop up server
alias serve='echo "ruby -run -e httpd . -p 4000" && ruby -run -e httpd . -p 4000'

# The space at the end is deliberate. It prevents saving to shell history.
alias switchoff='/usr/bin/dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop '

# OSX
if [ $(uname) == Darwin ]; then
  alias ls='ls -G'
  alias tree='tree -C'
  alias less='less -R'
  alias tulpn='sudo lsof -nP -iTCP -sTCP:LISTEN'

  # Launch sublime in cwd
  alias subl="open -a 'sublime text' ."

  MONGOD_COMMAND="mongod --config /usr/local/etc/mongod.conf --fork"
  alias mongod.start='echo "$MONGOD_COMMAND" && $MONGOD_COMMAND'
fi

function photo_stream() {
  while :; do
    imagesnap ~/Desktop/$(date +%y%m%d%H%M%S).png
    sleep ${1-1}
  done
}

alias i="echo You\'re already in insert mode, you numpty."


############
#  Docker  #
############

alias docker-couchdb="docker run -t -d -p 5984:5984 --name couchdb klaemo/couchdb"
alias docker-postgres="docker run -t -d -p 5432:5432 --name postgres postgres"
