# Terminal
set --export EDITOR "/usr/bin/env nvim"
set --export TERM xterm

# Postgres config
set --export PGHOST localhost
set --export PGUSER postgres

set --export ANSIBLE_VAULT_PASSWORD_FILE "$HOME/.ansible/vault_password/honeycomb"

# Path
set fish_user_paths \
  "$HOME/.cache/rebar3/bin" \
  "$HOME/.cargo/bin" \
  "$HOME/.homesick/repos/homeshick/bin" \
  "$HOME/.local/bin" \
  "$HOME/.rbenv/bin" \
  "$HOME/bin"

  # "$HOME/.opam/system/bin" \


# Aliases

source "$HOME/.aliases.sh"

# Autojump
if test -e /usr/local/share/autojump/autojump.fish
  source /usr/local/share/autojump/autojump.fish
end
if test -e /usr/share/autojump/autojump.fish
  source /usr/share/autojump/autojump.fish
end

# rbenv ruby version manager
if test -e $HOME/.rbenv/bin/rbenv
  set PATH $HOME/.rbenv/bin $PATH
  set PATH $HOME/.rbenv/shims $PATH
  rbenv rehash >/dev/null ^&1
end

# Hub
if command --search /usr/local/share/autojump/autojump.fish >/dev/null
  eval (hub alias -s)
end

# Prompt

set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_color_branch green

set __fish_git_prompt_char_dirtystate '*'
set __fish_git_prompt_char_stagedstate '+'
set __fish_git_prompt_char_untrackedfiles '?'

function fish_prompt -d "Write out the prompt"
  set laststatus $status

  printf '%s%s %s%s%s%s%s' \
    (set_color green) $USER \
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
set -gx CAML_LD_LIBRARY_PATH "$HOME/.opam/system/lib/stublibs:/usr/local/lib/ocaml/stublibs";
