""""""""""""""""""""""""""
" Vundle package manager "
""""""""""""""""""""""""""
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

Bundle 'garbas/vim-snipmate'
Bundle 'honza/vim-snippets'
Bundle 'kien/ctrlp.vim'
Bundle 'marcweber/vim-addon-mw-utils'
Bundle 'scrooloose/syntastic'
Bundle 'slim-template/vim-slim'
Bundle 'tomtom/tlib_vim'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'wting/rust.vim'

filetype plugin indent on
filetype plugin on

""""""""""""""""
" Generic crap "
""""""""""""""""

" Allow switching buffers without saving current buffer
set hidden

" Syntax
syntax on
" Turn off syntax for long lines to improve performance
set synmaxcol=320

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

" don't redraw when performing macros (for performance)
set lazyredraw

"""""""""""""""""""""""""""""""""
" Indentation, Tabs, Space, Etc "
"""""""""""""""""""""""""""""""""

" Line wrapping
set wrap

set tabstop=8       "A tab is 8 spaces
set expandtab       "Always uses spaces instead of tabs
set softtabstop=2   "Insert 2 spaces when tab is pressed
set shiftwidth=2    "An indent is 2 spaces
set smarttab        "Indent instead of tab at start of line
set shiftround      "Round spaces to nearest shiftwidth multiple
set nojoinspaces    "Don't convert spaces to tabs

set autoindent
set smartindent

"""""""""""""""""""
" Persistent Undo "
"""""""""""""""""""
set undofile                " Save undo's after file closes
set undodir=~/.vim/undo     " where to save undo histories
set undolevels=1000         " How many undos
set undoreload=10000        " number of lines to save for undo

""""""""""""""""
" Highlighting "
""""""""""""""""
" Highlight chars in col 81 (long lines)
2mat ErrorMsg '\%80v.'
    " clear with :2mat

""""""""""""""""""
" Remapping keys "
""""""""""""""""""

let mapleader = ","

" Easier : commands!
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Have Y behave like D, C, etc (until end of line, not entire line)
nnoremap Y y$

" Have spacebar insert a space in normal mode
nnoremap <space> i<space><esc>l

" Turn off paste on leaving insert mode
" (only useful on machines with -clipboard where you have to use 'set paste')
au InsertLeave * set nopaste

" I've got used to the switched " and @ for buffers and macros on OSX.
" So lets swap those two feature mappings on other platforms (for UK keyboards)
if ! has('macunix')
  nnoremap @ "
  nnoremap " @
  vnoremap @ "
  vnoremap " @
endif

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

" Go away ex mode, you suck
nnoremap Q <Nop>


"""""""""""""""""""""""""""""""""""
" Nicer location list navigation! "
"""""""""""""""""""""""""""""""""""
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

"""""""""""""""""""
" Toggling stuff! "
"""""""""""""""""""
" Toggle Search result highlighting
nnoremap <F1> :set hlsearch!<CR>
" Toggle textwidth automatic new line insertion
nnoremap <silent> <F2> :exe "set textwidth=" . (&tw ? 0 : 79)<CR> <Bar> :echo ":set textwidth=" . &tw<CR>
" Toggle show hidden chars
noremap <F3> :set list!<CR>
" Toggle spell check
noremap <F4> :setlocal spell!<CR>
" Toggle syntax check
noremap <F5> :SyntasticToggleMode<CR>

""""""""""""""""""""""
" HTML char escaping "
""""""""""""""""""""""
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

"""""""""""""""""""""""""""""""""""""""""""""""
" plugin: Syntastic syntax checking behaviour "
"""""""""""""""""""""""""""""""""""""""""""""""

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

" Ruby: mri syntax, rubocop style
let g:syntastic_ruby_checkers=['mri', 'rubocop']

""""""""""""""""
" ctrlp plugin "
""""""""""""""""

" Start in regexp mode
let g:ctrlp_regexp = 1

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.git|\.hg|\.svn|_site)$',
  \ }

" vim-commentary plugin

autocmd FileType slim set commentstring=/\ %s
