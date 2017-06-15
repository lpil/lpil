augroup filetype_ocaml
  autocmd!
  autocmd BufWritePre *.ml Neoformat
  autocmd BufWritePre *.mli Neoformat
augroup END
