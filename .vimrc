""""""""""""""""""""""""""
" Vundle package manager "
""""""""""""""""""""""""""
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/Vundle.vim'

Bundle 'bitc/vim-hdevtools'
Bundle 'cespare/vim-toml'
Bundle 'chrisbra/Colorizer'
Bundle 'digitaltoad/vim-jade'
Bundle 'editorconfig/editorconfig-vim'
Bundle 'godlygeek/tabular'
Bundle 'guns/vim-sexp'
Bundle 'honza/vim-snippets'
Bundle 'jelera/vim-javascript-syntax'
Bundle 'justinmk/vim-sneak'
Bundle 'kien/ctrlp.vim'
Bundle 'lpil/my-vim-snippets'
Bundle 'lpil/vim2hs-flexible'
Bundle 'marcweber/vim-addon-mw-utils'
Bundle 'rking/ag.vim'
Bundle 'sbl/scvim'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'Shougo/neocomplcache.vim'
Bundle 'SirVer/ultisnips'
Bundle 'slim-template/vim-slim'
Bundle 'sophacles/vim-processing'
Bundle 'tomtom/tlib_vim'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-sexp-mappings-for-regular-people'
Bundle 'tpope/vim-surround'
Bundle 'typedclojure/vim-typedclojure'
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

" Default folds are no fun
set nofoldenable

" Delete comment character when joining commented lines
if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j
endif

"""""""""""""""""""""""""""""""""
" Indentation, Tabs, Space, Etc "
"""""""""""""""""""""""""""""""""

set wrap

set autoindent
set smartindent

set expandtab       "Always uses spaces instead of tabs
set nojoinspaces    "Don't convert spaces to tabs
set shiftround      "Round spaces to nearest shiftwidth multiple
set smarttab        "Indent instead of tab at start of line

set shiftwidth=2    "An indent is 2 spaces
set softtabstop=2   "Insert 2 spaces when tab is pressed
set tabstop=2       "A tab is 2 spaces

" Lang specific
autocmd FileType python setlocal ts=4 sw=4

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
" Highlight chars in col 80
2mat ErrorMsg '\%80v.'
    " clear with :2mat

""""""""""""""""""
" Filetype stuff "
""""""""""""""""""

autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.pde set filetype=processing

""""""""""""""""""
" Remapping keys "
""""""""""""""""""

let mapleader = " "

" Easier : commands!
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
nnoremap q; q:

" Have Y behave like D, C, etc (until end of line, not entire line)
nnoremap Y y$

" Turn off paste on leaving insert mode
" (only useful on machines with -clipboard where you have to use 'set paste')
au InsertLeave * set nopaste

" I've got used to the switched " and @ for buffers and macros on OSX.
" So lets swap those two mappings. Use a UK keymap for OSX
nnoremap @ "
nnoremap " @
vnoremap @ "
vnoremap " @

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

" Fast buffer switching
nnoremap , :bn<CR>
" Fast buffer closing
nnoremap d, :bd<CR>

" Go away ex mode, you suck
nnoremap Q <Nop>
" No, I don't want to look things up in the man pages
nnoremap K <Nop>

" Undo in insert mode
inoremap <C-U> <C-G>u<C-U>

"""""""""""""""""""
" Toggling stuff! "
"""""""""""""""""""
" Toggle Search result highlighting
nnoremap <F1> :set hlsearch!<CR>
" Toggle textwidth automatic new line insertion
nnoremap <silent> <F2> :exe "set textwidth=" . (&tw ? 0 : 79)<CR> <Bar> :echo ":set textwidth=" . &tw<CR>
" Toggle show hidden chars
nnoremap <F3> :set list!<CR>
" Toggle spell check
nnoremap <F4> :setlocal spell!<CR>
" Toggle syntax check
nnoremap <F5> :SyntasticToggleMode<CR>

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

"""""""""""""""""""""""""""""
" Strip trailing whitespace "
"""""""""""""""""""""""""""""
function! StripWhitespace()
  let save_cursor = getpos(".")
  let old_query = getreg('/')
  :%s/\s\+$//e
  call setpos('.', save_cursor)
  call setreg('/', old_query)
endfunction
noremap <leader>d<space> :call StripWhitespace()<CR>

"""""""""""""""""""""""""
"  Convert ruby hashes  "
"""""""""""""""""""""""""
autocmd FileType ruby nnoremap <leader>rh :%s/:\(\w\+\)\(\s*=>\s*\)/\1: /g<CR>

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

"""""""""""""""""""""
" Reverse selection "
"""""""""""""""""""""

if has('macunix')
  command -range=% Reverse :'<,'>!gtac
else
  command -range=% Reverse :'<,'>!tac
endif

"""""""""""""""""""""""""""""""""""""""""""""""
" plugin: Syntastic syntax checking behaviour "
"""""""""""""""""""""""""""""""""""""""""""""""

" On by default, except for html
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'passive_filetypes': ['html'] }

" error highlighting
let g:syntastic_enable_highlighting = 1

let g:syntastic_enable_signs = 0

" Have the error list automatically close
let g:syntastic_auto_loc_list = 1

let g:syntastic_check_on_wq = 0

let g:syntastic_ruby_checkers=['mri', 'rubocop']
let g:syntastic_haskell_checkers=['hdevtools', 'hlint', 'ghc_mod']

""""""""""""""""""
" plugin: ctrlp "
""""""""""""""""""

" Search MRU, buffers, and files
nnoremap <C-o> :CtrlPMixed<CR>

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.git|\.hg|\.svn|_site|node_modules)$',
  \ }

"""""""""""""""""""""""""""
" plugin : vim-commentary "
"""""""""""""""""""""""""""

autocmd FileType javascript set commentstring=//\ %s
autocmd FileType lhaskell set commentstring=>\ %s
autocmd FileType processing set commentstring=//\ %s
autocmd FileType slim set commentstring=/\ %s
autocmd FileType supercollider set commentstring=//\ %s

"""""""""""""""""
" Plugin: Sneak "
"""""""""""""""""

let g:sneak#s_next = 1

"""""""""""""""""""
" Plugin: tabular "
"""""""""""""""""""

nnoremap <leader>tt :Tab /=<CR>
vnoremap <leader>tt :Tab /=<CR>
nnoremap <leader>t= :Tab /=<CR>
vnoremap <leader>t= :Tab /=<CR>

"""""""""""""""""""""""""""""""""""""""""
" Plugin: Colorizer (terrible spelling) "
"""""""""""""""""""""""""""""""""""""""""

let g:colorizer_auto_filetype='css,scss'

"""""""""""""""""""""""""
" Plugin: neocomplcache "
"""""""""""""""""""""""""

let g:neocomplcache_enable_at_startup = 1

"""""""""""""""""""""
" Plugin: ultisnips "
"""""""""""""""""""""

let g:snips_author="Louis Pilfold"

" Simulate textmate field selection behaviour
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

""""""""""""""""""
" Plugin: vim2hs "
""""""""""""""""""

au FileType haskell hi Conceal ctermbg=NONE guifg=NONE guibg=NONE

"""""""""""""""""""""
" Plugin: fireplace "
"""""""""""""""""""""

au FileType clojure nnoremap <leader>r :w<CR>:Require<CR>
au FileType clojure nnoremap cpo :Eval<CR>

""""""""""""""
"  NERDtree  "
""""""""""""""
nnoremap <leader>n :NERDTreeToggle<CR>

" close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"""""""""""""""""""""""""""""""""""""
" And finally, per-project .vimrc's "
"""""""""""""""""""""""""""""""""""""

" Disable unsafe commands in local .vimrc files
set secure
" Enable reading of a .vimrc found in the current dir
set exrc
