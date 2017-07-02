" Automatically reload a buffer when the file is detected as
" having changed on disk, presuming the buffer is not dirty.
set autoread

" Check for files changes when a buffer gains focus
autocmd FocusGained * checktime
