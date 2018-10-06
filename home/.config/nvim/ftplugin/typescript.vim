set commentstring=//\ %s

let g:jsx_ext_required = 0

augroup filetype_typecript
  autocmd!
  autocmd BufWritePre *.ts Neoformat
augroup END
