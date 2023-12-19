function! Open3Columns()
  vsplit
  vsplit
  wincmd l
  wincmd l
  split
  terminal
  wincmd j
  terminal
  wincmd k
endfunction

command! ThreeColumns call Open3Columns()

function! Open2Columns()
  vsplit
  split
  terminal
endfunction

command! TwoColumns call Open2Columns()
