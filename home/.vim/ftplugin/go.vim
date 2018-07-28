augroup filetype_go
  autocmd!
  autocmd BufWritePre *.go Neoformat
augroup END

" Leave this to Neocomplete
let g:go_fmt_autosave=0

set shiftwidth=4
set softtabstop=4
set tabstop=4
