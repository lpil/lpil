" Convert hashs from old syntax
autocmd FileType ruby nnoremap <leader>rh :%s/:\(\w\+\)\(\s*=>\s*\)/\1: /g<CR>
