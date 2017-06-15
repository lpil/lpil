set commentstring=//\ %s

let g:jsx_ext_required = 0

augroup filetype_javascript
  autocmd!
  autocmd BufWritePre *.js Neoformat
augroup END
