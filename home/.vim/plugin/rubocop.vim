function! RubocopThisFile()
  :! rubocop -aD %
endfunction

command! RubocopThisFile call RubocopThisFile()
