alias bell='echo -e \\a'

alias up='time ping 8.8.8.8'

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

alias k="kubectl"

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
alias ber='bundle exec rails'

alias slideshow='feh --randomize --recursive --draw-filename --slideshow-delay 4 --image-bg black --auto-zoom'

# pop up server
alias serve='ruby -run -e httpd . -p 3000'

alias i="echo You\'re already in insert mode, you numpty."

alias transfer-random-data-new-music-to-media-usb-music="find data/new-music/*/* -maxdepth 0 | shuf | tr '\n' '\0' | xargs -0 -i cp -rv --no-clobber {} /media/usb/music/"

alias sorbet-counts="rg typed: --no-filename --no-line-number | sort | uniq -c"

alias get-docker-couchdb="docker run -t -d -p 5984:5984 --name couchdb klaemo/couchdb"
alias get-docker-elasticsearch-6.6="docker run -d --name elasticsearch-6.6 -p 9200:9200 -p 9300:9300 -e 'discovery.type=single-node' elasticsearch:6.6.1"
alias get-docker-mysql-5.7="docker run -t -d -p 3306:3306 --name mysql-5.7 -e MYSQL_ALLOW_EMPTY_PASSWORD=1 mysql:5.7 --bind-address=0.0.0.0"
alias get-docker-neo4j="docker run -t -d -e NEO4J_AUTH=none -p 7474:7474 -p 7473:7473 -p 7687:7687  --name neo4j neo4j"
alias get-docker-orientdb="docker run -e ORIENTDB_ROOT_PASSWORD=orientdb -t -d -p 2424:2424 -p 2480:2480 --name orientdb orientdb"
alias get-docker-postgres-9.4="docker run -t -d -p 5432:5432 --name postgres-9-4 postgres:9.4.15"
alias get-docker-postgres="docker run -t -d -p 5432:5432 --name postgres postgres"
alias get-docker-redis="docker run -t -d -p 6379:6379 --name redis redis --bind 0.0.0.0"
