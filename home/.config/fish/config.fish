# Terminal
set --export EDITOR "/usr/bin/env nvim"
set --export TERM xterm

# Disable greeting
set fish_greeting

# Postgres config
set --export PGHOST localhost
set --export PGUSER postgres
set --export PGPASSWORD postgres

# MySQL config
set --export MYSQL_HOST 127.0.0.1

# Erlang config
set --export ERL_AFLAGS "-kernel shell_history enabled"

# Erlang installer config
set --export KERL_INSTALL_MANPAGES true
set --export KERL_INSTALL_HTMLDOCS true

# Golang
set --export GOPATH $HOME/.local/go
set --export GOBIN $HOME/.local/bin

# Path
set fish_user_paths \
    "$GOBIN" \
    "$HOME/.asdf/shims" \
    "$HOME/.cache/rebar3/bin" \
    "$HOME/.cargo/bin" \
    "$HOME/.homesick/repos/homeshick/bin" \
    "$HOME/.local/bin" \
    "$HOME/bin"
# Aliases
source "$HOME/.aliases.sh"

# Autojump
if test -e /usr/local/share/autojump/autojump.fish
    source /usr/local/share/autojump/autojump.fish
else if test -e /usr/share/autojump/autojump.fish
    source /usr/share/autojump/autojump.fish
end

# asdf version manager
if test -e $HOME/.asdf/asdf.fish
    source $HOME/.asdf/asdf.fish
else
    echo $HOME/.asdf/asdf.fish not present
end

# Brew, rubbish package manager
if test -e /opt/homebrew
    set -gx HOMEBREW_PREFIX /opt/homebrew
    set -gx HOMEBREW_CELLAR /opt/homebrew/Cellar
    set -gx HOMEBREW_REPOSITORY /opt/homebrew
    set -q PATH; or set PATH ''
    set -gx PATH /opt/homebrew/bin /opt/homebrew/sbin $PATH
    set -q MANPATH; or set MANPATH ''
    set -gx MANPATH /opt/homebrew/share/man $MANPATH
    set -q INFOPATH; or set INFOPATH ''
    set -gx INFOPATH /opt/homebrew/share/info $INFOPATH
end

# zoxide
if type -q zoxide
    zoxide init fish | source
end

# Prompt

set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_color_branch green

set __fish_git_prompt_char_dirtystate '*'
set __fish_git_prompt_char_stagedstate '+'
set __fish_git_prompt_char_untrackedfiles '?'

function fish_prompt -d "Write out the prompt"
    set laststatus $status

    if set -q VIRTUAL_ENV
        printf "(%s) " (basename "$VIRTUAL_ENV")
    end

    printf '%s%s %s%s%s%s%s' \
        (set_color green) (echo $USER) \
        (set_color yellow) (echo $PWD | sed -e "s|^$HOME|~|") \
        (set_color white) (__fish_git_prompt) \
        (set_color white)
    if test $laststatus -eq 0
        printf " %s\$ %s" (set_color grey) (set_color normal)
    else
        printf " %s✘ %s\$ %s" (set_color -o red) (set_color grey) (set_color normal)
    end
end

# The next line updates PATH for the Google Cloud SDK.
set GOOGLE_SDK_PATH /home/louis/bin/google-cloud-sdk
if [ -f "$GOOGLE_SDK_PATH/path.fish.inc" ]
    source "$GOOGLE_SDK_PATH/path.fish.inc"
end

# https://github.com/rust-lang-nursery/rustfmt/issues/1687
if type -q rustc
    set --export LD_LIBRARY_PATH $LD_LIBRARY_PATH:(rustc --print sysroot)/lib
end

# libvips
set --export VIPSHOME /usr/local
set --export LD_LIBRARY_PATH $LD_LIBRARY_PATH:$VIPSHOME/lib

# FZF
# Use fd to list files for fzf as it respects gitignore
set --export FZF_DEFAULT_COMMAND fd

fish_add_path /opt/homebrew/opt/ruby/bin
fish_add_path /opt/homebrew/lib/ruby/gems/3.0.0/bin/

function dotenv --description 'Load environment variables from .env file'
    set -l envfile ".env"
    if [ (count $argv) -gt 0 ]
        set envfile $argv[1]
    end

    if test -e $envfile
        for line in (cat $envfile | grep -v "^#" | grep "=")
            set -xg (echo $line | cut -d = -f 1) (echo $line | cut -d = -f 2-)
        end
    end
end
