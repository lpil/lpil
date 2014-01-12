""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vundle package manager
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

Bundle 'scrooloose/syntastic'
Bundle 'justinmk/vim-sneak'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-repeat'
Bundle 'tmhedberg/matchit'
Bundle 'kien/ctrlp.vim'

filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Generic crap
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Allow switching buffers without saving current buffer
set hidden

" Syntax
syntax on

" Colors
set background=dark

" Set it to scroll when cursor is # lines from top/bottom
set so=7

" Mouse support
set mouse=a

" Line numbers
set nu
highlight LineNr ctermfg=Brown
set ruler

" Improve backspace deletion behaviour
set backspace=indent,eol,start

" Smart case insensitive search
set ignorecase
set smartcase

" / searches before hitting enter
set incsearch

" Command line auto complete
set wildmenu

" Auto read external file changes
set autoread

" Colours
set t_Co=256

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Indentation, Tabs, Space, Etc
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Line wrapping
set wrap
set tw=79

set tabstop=8                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=2               "Insert 2 spaces when tab is pressed
set shiftwidth=2                "An indent is 2 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs

set autoindent
set smartindent

filetype plugin on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Highlighting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Highlight chars in col 81 (long lines)
2mat ErrorMsg '\%81v.'
    " clear with :2mat

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remapping keys
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let mapleader = ","

" Easier : commands!
nnoremap ; :
" Get ; functionality back
nnoremap : ;

" Have spacebar insert a space in normal mode
nnoremap <space> i<space><esc>l

" Set F2 to allow pasting from system clipboard without vim intending it
set pastetoggle=<F2>
" Turn off paste on leaving insert mode
au InsertLeave * set nopaste

" Nicer location list navigation. Calls the function below.
nnoremap <leader>[ :call WrapLnext("up")<CR>
nnoremap <leader>] :call WrapLnext("down")<CR>

" This function is a replacement for :lnext and :lprevious. It allows you to
" wrap around from last to first (and back again)
function! WrapLnext(direction)
  if a:direction == "up"
    try
      lprevious
    catch /^Vim\%((\a\+)\)\=:E553/
      llast
    endtry
  elseif a:direction == "down"
    try
      lnext
    catch /^Vim\%((\a\+)\)\=:E553/
      lfirst
    endtry
  endif
endfunction

" Nicer split navigation 
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-left> <C-w>h
nnoremap <C-down> <C-w>j
nnoremap <C-up> <C-w>k
nnoremap <C-right> <C-w>l

" Improve up/down movement on wrapped lines
nnoremap j gj
nnoremap k gk

" Easier increment/decrement
nnoremap + <C-a>
nnoremap - <C-x>

" Toggle Search result highlighting
nnoremap <F1> :set hlsearch!<CR>
" Toggle textwidth automatic new line insertion
nnoremap <silent> <F2> :exe "set textwidth=" . (&tw ? 0 : 79)<CR> <Bar> :echo ":set textwidth=" . &tw<CR>
" Toggle show hidden chars
noremap <F3> :set list!<CR>
" Toggle spell check
noremap <F4> :setlocal spell!

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HTML char escaping
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" This will escape HTML chars from the last pasted block
nnoremap <Leader>h :'[,']call HtmlEscape()<CR>
" This will do it for the visually selected block
vnoremap <Leader>h :call HtmlEscape()<CR>

" Insert more chars to replace here
function HtmlEscape()
  silent s/&/\&amp;/eg  " Amp has to be first!
  silent s/</\&lt;/eg   " less than
  silent s/>/\&gt;/eg   " greater than
  silent s/\ /\%20;/eg  " space
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Macros
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" don't redraw when performing macros (for performance)
set lazyredraw

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" plugin: Syntastic syntax checking behaviour
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" error highlighting
let g:syntastic_enable_highlighting = 0

" signs (markers on the left)
let g:syntastic_enable_signs = 1
let g:syntastic_error_symbol = 'XX'
let g:syntastic_warning_symbol = '??'

" Have the error list automatically close
let g:syntastic_auto_loc_list = 1

" checking on :wq
let g:syntastic_check_on_wq = 0
