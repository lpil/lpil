setlocal formatprg=hindent

let g:hindent_on_save = 1
let g:hindent_indent_size = 2
let g:hindent_line_length = 80

nnoremap <Leader>lm :InteroLoadCurrentModule<CR>
nnoremap <Leader>r :InteroReload<CR>
nnoremap <Leader>w :w<CR>:InteroReload<CR>

nnoremap <Leader>e :InteroEval<CR>
nnoremap <Leader>t :InteroGenericType<CR>
nnoremap <Leader>it :InteroTypeInsert<CR>

" Go to definition:
nnoremap <c-]> :InteroGoToDef<CR>

augroup filetype_haskell
  autocmd!
  autocmd BufWritePre *.hs Neoformat
augroup END
