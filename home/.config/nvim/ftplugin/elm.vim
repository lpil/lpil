setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

augroup filetype_elm
  autocmd!
  autocmd BufWritePre *.elm Neoformat
augroup END
