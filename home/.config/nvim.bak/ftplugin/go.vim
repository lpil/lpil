augroup filetype_go
  autocmd!
  autocmd BufWritePre *.go Neoformat
augroup END

" Leave this to Neocomplete
let g:go_fmt_autosave=0

" Use location list instead of quickfix
let g:go_list_type = "locationlist"

set shiftwidth=8
set softtabstop=8
set tabstop=8

nnoremap <leader>w :w<CR>:GoBuild<CR>
