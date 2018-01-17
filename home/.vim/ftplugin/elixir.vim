let g:neoformat_elixir_exfmt = {
  \ 'exe': 'mix',
  \ 'args': ['format'],
  \ 'replace': 1,
  \ }

let g:neoformat_enabled_elixir = ['exfmt']

augroup filetype_elixir
  autocmd!
  autocmd BufWritePre *.ex Neoformat
  autocmd BufWritePre *.exs Neoformat
augroup END
