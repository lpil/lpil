" Search API docs for word under cursor:
nnoremap <silent> K :call Dasht([expand('<cWORD>'), expand('<cword>')])<Return>

" Search API docs for the selected text:
vnoremap <silent> K y:<C-U>call Dasht(getreg(0))<Return>

" Specify additional API docs to search:
" (maps filetype name to docset regexps)
let g:dasht_filetype_docsets = {
      \ 'elixir': ['erlang'],
      \ 'html': ['css', 'js'],
      \ 'javascript': ['nodejs', 'react'],
      \ }
