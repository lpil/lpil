let g:neoformat_scala_filescalafmt = {
  \ 'exe': 'scalafmt',
  \ 'replace': 1,
  \ }

let g:neoformat_enabled_scala = ['filescalafmt']

augroup filetype_scala
  autocmd!
  autocmd BufWritePre *.scala Neoformat
augroup END
