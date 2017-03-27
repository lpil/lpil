setlocal formatprg=hindent

let g:hindent_on_save = 1
let g:hindent_indent_size = 2
let g:hindent_line_length = 80

nnoremap <leader>a :GhcModTypeInsert<CR>
nnoremap <leader>s :GhcModSplitFunCase<CR>
nnoremap <leader>t :GhcModType<CR>
