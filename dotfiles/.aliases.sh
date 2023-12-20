alias bell='echo -e \\a'

alias up='time ping 8.8.8.8'

alias w='watchexec'

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

alias grep='grep -i --color=auto'
alias tree='tree -C'
alias less='less -R'
alias watch='watch -c'

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

alias dnslookup="dig +noall +answer"

# Sorbet: Ruby type checker
alias sorbet-counts="rg typed: --no-filename --no-line-number | sort | uniq -c"
alias sorbet-ignored="rg typed:\ ignore | fpp"

# Run things in containers
alias get-podman-mariadb="podman run -t -d -p 3306:3306 --name mariadb -e MYSQL_ALLOW_EMPTY_PASSWORD=1 mariadb --bind-address=0.0.0.0"
alias get-podman-postgres="podman run -t -d -p 5432:5432 -e POSTGRES_PASSWORD=postgres --name postgres postgres"
alias get-podman-redis="podman run -t -d -p 6379:6379 --name redis redis --bind 0.0.0.0"
