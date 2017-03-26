setlocal formatprg=hindent
autocmd BufWritePre *.hs silent exe ":%!hindent"

nnoremap <leader>a :GhcModTypeInsert<CR>
nnoremap <leader>s :GhcModSplitFunCase<CR>
nnoremap <leader>t :GhcModType<CR>
