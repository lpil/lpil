# Terminal
set --export EDITOR "/usr/bin/env nvim"
set --export TERM xterm

# Disable greeting
set fish_greeting

# Postgres config
set --export PGHOST localhost
set --export PGUSER postgres
set --export PGPASSWORD postgres

# Erlang config
set --export ERL_AFLAGS "-kernel shell_history enabled"

# Erlang installer config
set --export KERL_INSTALL_MANPAGES true
set --export KERL_INSTALL_HTMLDOCS true

# Golang
set --export GOPATH $HOME/.local/go
set --export GOBIN $HOME/.local/bin

# Python
set --export PYTHONPATH "/usr/bin/env python"

# asdf version manager
source $HOME/.asdf/asdf.fish

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

if test -e /Applications/Tailscale.app/Contents/MacOS/Tailscale
    alias tailscale="/Applications/Tailscale.app/Contents/MacOS/Tailscale"
end


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

if type -q zoxide
    zoxide init fish | source
end

if type -q zoxide
    direnv hook fish | source
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

# https://github.com/rust-lang-nursery/rustfmt/issues/1687
if type -q rustc
    set --export LD_LIBRARY_PATH $LD_LIBRARY_PATH:(rustc --print sysroot)/lib
end

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

# mpd
set --export MPD_HOST "$HOME/.mpd/socket"
