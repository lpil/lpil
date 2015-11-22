if has('nvim')
  runtime! plugin/python_setup.vim
  tnoremap <Esc> <C-\><C-n>
endif

call plug#begin('~/.vim/plugged')

Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'digitaltoad/vim-jade', { 'for': 'jade' }
Plug 'elixir-lang/vim-elixir', { 'for': ['elixir', 'markdown'] }
Plug 'godlygeek/tabular'
Plug 'honza/vim-snippets'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'jpalardy/vim-slime'
Plug 'junegunn/vader.vim',  { 'on': 'Vader', 'for': 'vader' }
Plug 'kien/ctrlp.vim'
Plug 'marcweber/vim-addon-mw-utils'
Plug 'munshkr/vim-tidal'
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'rhysd/clever-f.vim'
Plug 'sbl/scvim', { 'for': 'supercollider' }
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'scrooloose/syntastic'
Plug 'shougo/neocomplcache.vim'
Plug 'sirver/ultisnips'
Plug 'slim-template/vim-slim'
Plug 'tomtom/tlib_vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-cucumber', { 'for': 'cucumber' }
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/matchit.zip'
Plug 'wting/rust.vim', { 'for': 'rust' }

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

autocmd BufNewFile,BufRead Guardfile set filetype=ruby
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.pde set filetype=processing
autocmd BufRead,BufNewFile *.scss set filetype=scss.css

" Highlight fenced code blocks in markdown docs
let g:markdown_fenced_languages = [
      \'sh',
      \'css',
      \'elixir',
      \'haskell',
      \'html',
      \'javascript',
      \'js=javascript',
      \'json=javascript',
      \'python',
      \'ruby',
      \'sass',
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

" Fast buffer switching
nnoremap , :bn<CR>
" Fast buffer closing
nnoremap d, :bd<CR>

" Go away ex mode, you suck
nnoremap Q <Nop>

" No, I don't want to look things up in the man pages
nnoremap K <Nop>
vnoremap K <Nop>

" Let's not accidentally open help all the time
inoremap <F1> <Nop>

" Undo in insert mode
inoremap <C-U> <C-G>u<C-U>

" Open vimrc to edit
nnoremap <leader>ev :e $MYVIMRC<cr>
" Reload vimrc
nnoremap <leader>sv :source $MYVIMRC<cr>

"""""""""""""""""""
" Toggling stuff! "
"""""""""""""""""""

nnoremap <F1> :set hlsearch!<CR>
nnoremap <F2> :NERDTreeToggle<CR>
nnoremap <F3> :BuffergatorToggle<CR>
nnoremap <F4> :setlocal spell!<CR>
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

"""""""""""""""""""""
" Reverse selection "
"""""""""""""""""""""

if has('macunix')
  command! -range=% Reverse :'<,'>!gtac
else
  command! -range=% Reverse :'<,'>!tac
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

" let g:syntastic_ruby_checkers=['mri', 'rubocop']
let g:syntastic_ruby_checkers=['mri']


""""""""""""""""""
" plugin: ctrlp "
""""""""""""""""""

" Search MRU, buffers, and files
nnoremap <C-o> :CtrlPMixed<CR>

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.git|\.hg|\.svn|_site|node_modules|_build|coverage|deps|tmp)$',
  \ }


"""""""""""""""""""
" Plugin: tabular "
"""""""""""""""""""

nnoremap <leader>tt :Tab /=<CR>
vnoremap <leader>tt :Tab /=<CR>
nnoremap <leader>t= :Tab /=<CR>
vnoremap <leader>t= :Tab /=<CR>


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


""""""""""""""
"  NERDtree  "
""""""""""""""
nnoremap <leader>n :NERDTreeToggle<CR>
let g:NERDTreeWinSize=40


"""""""""""
"  slime  "
"""""""""""

let g:slime_target = "tmux"
let g:slime_no_mappings = 1
xmap <C-e> <Plug>SlimeRegionSend
nmap <C-e> <Plug>SlimeParagraphSend

"""""""""""""""""""""""""""""""""""""
" And finally, per-project .vimrc's "
"""""""""""""""""""""""""""""""""""""

" Disable unsafe commands in local .vimrc files
set secure
" Enable reading of a .vimrc found in the current dir
set exrc
