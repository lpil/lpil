"""""""""""""""""""
"  Abbreviations  "
"""""""""""""""""""

" Abbreviations are like mappings, but for insert mode, and a bit more
" automatic.

iabbrev adn and

" Now if you type `adn ` you'll get `and `

" Vim will substitute an abbreviation when you type any "non-keyword
" character" after an abbreviation.

set iskeyword?

" iskeyword=@,48-57,_,192-255
" @s, underscores, ascii alphanumerics, and a few other ascii chars.
