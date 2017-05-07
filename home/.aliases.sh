alias bell='echo -e \\a'

alias up='time ping 4.2.2.2'

alias x='startx'

alias v='vim'
alias n='nvim'

alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias lla='ls -lah'

alias c='cd'

alias g='git'
alias ga='git add .; and git status'
alias gs='git status'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias dl='cd ~/downloads'

alias grep='grep -i --color=auto'
alias fgrep='fgrep -i --color=auto'
alias egrep='egrep -i --color=auto'
alias tree='tree -C'

alias less='less -R'

# Case insensitive locate
alias locate='locate -i'

# feh images fit window size
alias feh='feh -.'

# ruby: bundle exec shorthands
alias be='bundle exec'
alias ber='bundle exec rake'

# pop up server
alias serve='ruby -run -e httpd . -p 4000'

alias i="echo You\'re already in insert mode, you numpty."

alias transfer-random-data-new-music-to-media-usb-music="find data/new-music/*/* -maxdepth 0 | shuf | tr '\n' '\0' | xargs -0 -i cp -rv --no-clobber {} /media/usb/music/"

alias get-docker-couchdb="docker run -t -d -p 5984:5984 --name couchdb klaemo/couchdb"
alias get-docker-postgres="docker run -t -d -p 5432:5432 --name postgres postgres"
alias get-docker-redis="docker run -t -d -p 6379:6379 --name redis redis"
