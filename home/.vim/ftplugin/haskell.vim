setlocal formatprg=hindent

let g:hindent_on_save = 1
let g:hindent_indent_size = 2
let g:hindent_line_length = 80

nnoremap <leader>a :GhcModTypeInsert<CR>
nnoremap <leader>s :GhcModSplitFunCase<CR>
nnoremap <leader>t :GhcModType<CR>

" Inteo Process management:
nnoremap <Leader>hio :InteroOpen<CR>
nnoremap <Leader>hik :InteroKill<CR>
nnoremap <Leader>hic :InteroHide<CR>
nnoremap <Leader>hil :InteroLoadCurrentModule<CR>

" REPL commands
nnoremap <Leader>hie :InteroEval<CR>
nnoremap <Leader>hit :InteroGenericType<CR>
nnoremap <Leader>hiT :InteroType<CR>
nnoremap <Leader>hii :InteroInfo<CR>
nnoremap <Leader>hiI :InteroTypeInsert<CR>

" Go to definition:
nnoremap <Leader>hid :InteroGoToDef<CR>

" Highlight uses of identifier:
nnoremap <Leader>hiu :InteroUses<CR>

augroup filetype_haskell
  autocmd!
  autocmd BufWritePre *.hs Neoformat

  " Reload the file in Intero after saving
  autocmd! BufWritePost *.hs InteroReload
augroup END
