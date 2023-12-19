let g:snips_author="Louis Pilfold"

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

augroup load_vendor_snippets
  autocmd!
  autocmd FileType javascript UltiSnipsAddFiletypes javascript-mocha
augroup END
