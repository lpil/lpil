augroup filetype_rust
  autocmd!
  autocmd BufWritePre *.rs Neoformat
augroup END

let g:rustfmt_autosave=0

set shiftwidth=4
set softtabstop=4
set tabstop=4
