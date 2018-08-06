augroup filetype_go
  autocmd!
  autocmd BufWritePre *.go Neoformat
augroup END

" Leave this to Neocomplete
let g:go_fmt_autosave=0

" Use location list instead of quickfix
let g:go_list_type = "locationlist"

" Don't open scratch preview buffer for completion options
set completeopt-=preview

set shiftwidth=4
set softtabstop=4
set tabstop=4

nnoremap <leader>w :w<CR>:GoBuild<CR>
