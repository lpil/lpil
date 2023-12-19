augroup filetype_ocaml
  autocmd!
  autocmd BufWritePre *.ml Neoformat
  autocmd BufWritePre *.mli Neoformat
augroup END

set commentstring=(*%s*)

" Enable Merlin based completion
if !exists('g:deoplete#omni_patterns')
  let g:deoplete#omni#input_patterns = {}
endif
let g:deoplete#omni#input_patterns.ocaml = '[^. *\t]\.\w*|\s\w*|#'

" Open docs of word under cursor
nnoremap <buffer> K :MerlinDocument<CR>

" Jump to definition of item under cursor
nnoremap <buffer> <C-]> :MerlinLocate<CR>
