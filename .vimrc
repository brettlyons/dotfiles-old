" vim-plug plugin loader
call plug#begin('~/.vim/plugged')

" Sensible vim config
Plug 'tpope/vim-sensible'
" Great for Vim, unnecessary for NeoVim

" Vim-surround -- awesome
Plug 'tpope/vim-surround'

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Syntastic
Plug 'scrooloose/syntastic'

" SuperTab
Plug 'iervandew/supertab'

" Ultisnips w/snippets
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" Deoplete (hope it doesn't conflict with ultisnips?)
" Plug 'Shougo/deoplete.nvim', has('nvim') ? {} : { 'on' : [], 'do' : ':UpdateRemotePlugins' }

"YouCompleteMe
" on new installs, need to nav to the dir and ./install
Plug 'valloric/youcompleteme', { 'do' : './install.sh' }

" NERD commenter
Plug 'scrooloose/nerdcommenter'

" NERD tree
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" Auto-pairs
Plug 'jiangmiao/auto-pairs'

"Emmet mode
Plug 'mattn/emmet-vim', { 'for' : ['html', 'css'] }

" Clojure setup
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" JS completion with TERN-js
Plug 'carlitux/deoplete-ternjs'

" Elm-Mode for elm files
Plug 'elmcast/elm-vim', { 'for': 'elm' }

" Scala Mode / Ensime
Plug 'derekwyatt/vim-scala'
Plug 'ensime/ensime-vim'

" Elixir mode
Plug 'elixir-lang/vim-elixir'

" Vim-parinfer
Plug 'bhurlow/vim-parinfer', { 'do': 'npm i' }

" Material theme
" Plug 'jdkanani/vim-material-theme'

" Solarized theme
" Plug 'altercation/vim-colors-solarized'

call plug#end()

filetype plugin indent on
syntax on
set splitright " prefer right-splits
set splitbelow  " and bottom splits

set background=dark
" colorscheme material-theme

" airline config
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='luna'

let g:ycm_semantic_triggers = {
     \ 'elm' : ['.'],
     \}

" Synstastic setup
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1

let g:elm_syntastic_show_warnings = 1

" color the 80th column please.
if (exists('+colorcolumn'))
  set colorcolumn=80
  highlight ColorColumn ctermbg=2
endif

" NeoVim Specifics
if has('nvim')
  " Deoplete
  "let g:deoplete#enable_at_startup = 1
  "let g:deoplete#enable_smart_case = 1
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsListSnippets='<c-tab>'
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" ultisnip config
" let g:UltiSnipsExpandTrigger='<tab>'
" let g:UltiSnipsJumpForwardTrigger='<c-j>'
" let g:UltiSnipsJumpBackwardTrigger='<c-k>'

" Emmet Mode for HTML/CSS only
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

" sets save(write) on dbl-escape
map <Esc><Esc> :w<CR>

" Leader key = backslash
let mapleader = '\'

" toggle nerd tree on Ctl-\
map <C-\> :NERDTreeToggle<CR>
map <leader>\ :NERDTreeToggle<CR>
" Show matching pairs
set showmatch

" turn off the visual bell
set visualbell t_vb=

" Disable all cursor blinking:
set guicursor+=a:blinkon0
" Default setting, uncomment to restore:
" :set guicursor&

" Enable mouse for all modes
set mouse=a

" tab stuffs
set number    "sets absolute line numbers
" :set relativenumber     "sets relative line numbers, when combined with the above, gives 2 columns of line numbers
set autoindent         "continues indentation from previous line
" :set smartindent        "adds indentation sometimes
set tabstop=2   "a tab is 2 spaces
set expandtab   "Always use spaces instead of tabs
set softtabstop=2 "Insert 2 spaces per tab press
set shiftwidth=2  "An indent is 2 spaces
set shiftround    "Round indent to nearest shiftwidth multiple
:%retab!
nnoremap <F2> :<C-U>setlocal lcs=tab:>-,trail:-,eol:$ list! list? <CR>

fun! <SID>StripTrailingWhiteSpaces()
  let l = line(".")
  let c = col(".")
  %s/\s\+$//e
  call cursor(l, c)
endfun

" Typecheck the scala file on save.
" autocmd BufWritePost *.scala :EnTypeCheck " for scala typechecking.

autocmd BufWritePre * :call <SID>StripTrailingWhiteSpaces()
