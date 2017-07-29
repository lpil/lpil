if has('nvim')
  runtime! plugin/python_setup.vim
  tnoremap <Esc> <C-\><C-n>
endif

function! DoRemote(arg)
  UpdateRemotePlugins
endfunction

call plug#begin('~/.vim/plugged')

let lisp_languages = ['scheme', 'clojure', 'lfe']

Plug 'cakebaker/scss-syntax.vim', { 'for': ['scss', 'scss.css'] }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dag/vim-fish', { 'for': 'fish' }
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'digitaltoad/vim-pug', { 'for': ['pug', 'jade'] }
Plug 'eagletmt/ghcmod-vim', { 'for' : 'haskell' }
Plug 'elixir-lang/vim-elixir', { 'for': ['elixir', 'markdown'] }
Plug 'elmcast/elm-vim', { 'for': 'elm' }
Plug 'evanmiller/nginx-vim-syntax', { 'for': 'nginx' }
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'flowtype/vim-flow', { 'for': ['javascript'] }
Plug 'foosoft/vim-argwrap', { 'on': 'ArgWrap' }
Plug 'frigoeu/psc-ide-vim', { 'for': 'purescript' }
Plug 'godlygeek/tabular'
Plug 'guns/vim-sexp', { 'for': lisp_languages }
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'honza/vim-snippets'
Plug 'idris-hackers/idris-vim', { 'for': 'idris' }
Plug 'jparise/vim-graphql', { 'for': 'graphql' }
Plug 'kylef/apiblueprint.vim', { 'for': 'apiblueprint' }
Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'lfe-support/vim-lfe', { 'for': 'lfe' }
Plug 'marcweber/vim-addon-mw-utils'
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'neovimhaskell/haskell-vim', { 'for' : 'haskell' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'parsonsmatt/intero-neovim', { 'for' : 'haskell' }
Plug 'powerman/vim-plugin-ansiesc', { 'for': 'elixir' } " Used by alchemist
Plug 'raichoo/purescript-vim', { 'for': 'purescript' }
Plug 'reasonml-editor/vim-reason', { 'for': 'reason' }
Plug 'rhysd/clever-f.vim'
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
Plug 'rust-lang/rust.vim', { 'for': ['rust', 'markdown'] }
Plug 'sbdchd/neoformat'
Plug 'sbl/scvim', { 'for': 'supercollider' }
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'shougo/vimproc.vim', { 'do' : 'make' } " used by ghcmod-vim
Plug 'sirver/ultisnips'
Plug 'slashmili/alchemist.vim', { 'for': 'elixir' }
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-cucumber', { 'for': 'cucumber' }
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': lisp_languages }
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-scripts/matchit.zip'

call plug#end()

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
highlight Normal ctermbg=none
highlight NonText ctermbg=none
highlight Normal guibg=none
highlight NonText guibg=none

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

" Disable search highlighting by default
set nohlsearch

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

" Preview regex changes
set inccommand=split

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

" Highlight fenced code blocks in markdown docs
let g:markdown_fenced_languages = [
      \'clojure',
      \'css',
      \'elixir',
      \'erlang',
      \'haskell',
      \'html',
      \'javascript',
      \'js=javascript',
      \'json=javascript',
      \'python',
      \'ruby',
      \'sass',
      \'scala',
      \'scheme',
      \'sh',
      \'xml'
      \]

""""""""""""""""""
" Remapping keys "
""""""""""""""""""

let mapleader = " "

" Easier : commands!
nnoremap ; :
vnoremap ; :
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

" Go away ex mode, you suck
nnoremap Q <Nop>

" Let's not accidentally open help all the time
inoremap <F1> <Nop>

" Undo in insert mode
inoremap <C-U> <C-G>u<C-U>

" Open vimrc to edit
nnoremap <leader>ev :e $MYVIMRC<cr>

" Generate ctags
nnoremap <leader>ct :! ctags -R .<cr>

"""""""""""""""""""
" Toggling stuff! "
"""""""""""""""""""

nnoremap <F1> :set hlsearch!<CR>
nnoremap <F4> :setlocal spell!<CR>
nnoremap <F5> :SyntasticToggleMode<CR>


" Reverse selection
if has('macunix')
  command! -range=% Reverse :'<,'>!gtac
else
  command! -range=% Reverse :'<,'>!tac
endif


""""""""""""""""""
" plugin: ctrlp "
""""""""""""""""""

" Search MRU, buffers, and files
nnoremap <C-o> :CtrlPMixed<CR>

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v[\/](\.git|\.hg|\.svn|_site|target|node_modules|bower_components|_build|dist|output|elm-stuff|coverage|deps|tmp)$',
  \ }

"""""""""""""""""""""""""
" Plugin: deocomplcache "
"""""""""""""""""""""""""

let g:deoplete#enable_at_startup = 1

"""""""""""""""""""""""""""""""""""""
" And finally, per-project .vimrc's "
"""""""""""""""""""""""""""""""""""""

" Disable unsafe commands in local .vimrc files
set secure
" Enable reading of a .vimrc found in the current dir
set exrc
