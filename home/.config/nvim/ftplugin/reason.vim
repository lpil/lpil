" let g:neoformat_reason_refmt = {
"   \ 'exe': 'refmt',
"   \ 'stdin': 1,
"   \ }

" let g:neoformat_enabled_reason = ['refmt']

augroup filetype_reason
  autocmd!
  " autocmd BufWritePre *.re Neoformat
augroup END

" Enable Merlin based completion
if !exists('g:deoplete#omni_patterns')
  let g:deoplete#omni#input_patterns = {}
endif
let g:deoplete#omni#input_patterns.reason = '[^. *\t]\.\w*|\s\w*|#'

" Open docs of word under cursor
nnoremap <buffer> K :MerlinDocument<CR>

" Jump to definition of item under cursor
nnoremap <buffer> <C-]> :MerlinLocate<CR>
