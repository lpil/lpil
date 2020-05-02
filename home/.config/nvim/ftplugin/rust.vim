augroup filetype_rust
  autocmd!
  autocmd BufWritePre *.rs Neoformat
augroup END

let g:neoformat_rust_rustfmt2018 = {
  \ 'exe': 'rustfmt',
  \ 'args': ['--edition', '2018'],
  \ 'stdin': 1,
  \ }

let g:neoformat_enabled_rust = ['rustfmt2018']

let g:rustfmt_autosave=0

set shiftwidth=4
set softtabstop=4
set tabstop=4
