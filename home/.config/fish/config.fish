# Terminal
set --export EDITOR "/usr/bin/env nvim"
set --export TERM xterm

# Disable greeting
set fish_greeting

# Postgres config
set --export PGHOST localhost
set --export PGUSER postgres
set --export PGPASSWORD postgres

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
  "$HOME/.cache/rebar3/bin" \
  "$HOME/.cargo/bin" \
  "$HOME/.homesick/repos/homeshick/bin" \
  "$HOME/.local/bin" \
  "$HOME/bin" \

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
  source ~/.asdf/asdf.fish
end

# Prompt

set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_color_branch green

set __fish_git_prompt_char_dirtystate '*'
set __fish_git_prompt_char_stagedstate '+'
set __fish_git_prompt_char_untrackedfiles '?'

# Nationwide messed up my user name.
function not_ugly_user_name
  if test $USER = louis.pilfordnationwide.co.uk
    echo "louis"
  else
    echo $USER
  end
end

function fish_prompt -d "Write out the prompt"
  set laststatus $status

  printf '%s%s %s%s%s%s%s' \
    (set_color green) (not_ugly_user_name) \
    (set_color yellow) (echo $PWD | sed -e "s|^$HOME|~|") \
    (set_color white) (__fish_git_prompt) \
    (set_color white)
  if test $laststatus -eq 0
    printf " %s\$ %s" (set_color grey) (set_color normal)
  else
    printf " %s✘ %s\$ %s" (set_color -o red) (set_color grey) (set_color normal)
  end
end

# OPAM configuration
set -gx OCAML_TOPLEVEL_PATH "$HOME/.opam/system/lib/toplevel";
set -gx PERL5LIB "$HOME/.opam/system/lib/perl5:$PERL5LIB";
set -gx MANPATH "$MANPATH" "$HOME/.opam/system/man";
set -gx OPAMUTF8MSGS "1";

# The next line updates PATH for the Google Cloud SDK.
set GOOGLE_SDK_PATH "/home/louis/bin/google-cloud-sdk"
if [ -f "$GOOGLE_SDK_PATH/path.fish.inc" ]
  source "$GOOGLE_SDK_PATH/path.fish.inc"
end

# https://github.com/rust-lang-nursery/rustfmt/issues/1687
if type -q rustc
  set --export LD_LIBRARY_PATH (rustc --print sysroot)/lib
end

# FZF
# Use fd to list files for fzf as it respects gitignore
set --export FZF_DEFAULT_COMMAND fd

# Path
set fish_user_paths \
  "$HOME/.cache/rebar3/bin" \
  "$GOBIN" \
  "$HOME/.cargo/bin" \
  "$HOME/.homesick/repos/homeshick/bin" \
  "$HOME/.local/bin" \
  "$HOME/bin"
