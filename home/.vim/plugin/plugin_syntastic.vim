" On by default, except for html
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'passive_filetypes': ['html'] }

" error highlighting
let g:syntastic_enable_highlighting = 1

let g:syntastic_enable_signs = 0

" Have the error list automatically close
let g:syntastic_auto_loc_list = 1

let g:syntastic_check_on_wq = 0

" let g:syntastic_ruby_checkers=['mri', 'rubocop']
let g:syntastic_ruby_checkers=['mri']
