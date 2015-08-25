set commentstring=//\ %s

let g:sclangKillOnExit=0

" Send outermost block
nnoremap <buffer> <C-d> :call SClang_block()<CR>
vnoremap <buffer> <C-d> :call SClang_send()<CR>

" Send current line/selection
vnoremap <buffer> <C-e> :call SClang_send()<CR>
nnoremap <buffer> <C-e> :call SClang_send()<CR>
