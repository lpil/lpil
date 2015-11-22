augroup set_filetype_group
  autocmd BufNewFile,BufRead Guardfile set filetype=ruby
  autocmd BufNewFile,BufRead *.md set filetype=markdown
  autocmd BufNewFile,BufRead *.pde set filetype=processing
  autocmd BufRead,BufNewFile *.scss set filetype=scss.css
augroup END
