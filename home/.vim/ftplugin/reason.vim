let g:neoformat_reason_refmt = {
  \ 'exe': 'refmt',
  \ 'stdin': 1,
  \ }

let g:neoformat_enabled_reason = ['refmt']

augroup filetype_reason
  autocmd!
  autocmd BufWritePre *.re Neoformat
augroup END
