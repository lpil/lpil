" " Write this in your vimrc file
let g:ale_lint_on_text_changed = 'never'
let g:ale_sign_column_always = 0

let g:ale_linters = {
  \   'ruby': ['rubocop'],
  \ }
