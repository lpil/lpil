
function! Open3Columns()
  vsplit
  vsplit
  split
  terminal
  wincmd j
  terminal
endfunction

command! ThreeColumns call Open3Columns()

function! Open2Columns()
  vsplit
  split
  terminal
  wincmd j
  terminal
endfunction

command! TwoColumns call Open2Columns()
