" Pathogen load
filetype off

call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on
syntax on

set background=dark
try
  colorscheme solarized
catch
endtry

if (exists('+colorcolumn'))
  set colorcolumn=80
  highlight ColorColumn ctermbg=2
endif

" sets save(write) on dbl-escape
map <Esc><Esc> :w<CR>

" turn off the visual bell
:set visualbell t_vb=

" Disable all cursor blinking:
:set guicursor+=a:blinkon0
" Default setting, uncommong to restore:
" :set guicursor&
" Enable mouse for all modes
:set mouse=a

" tab stuffs
:set number 		"sets absolute line numbers
" :set relativenumber     "sets relative line numbers, when combined with the above, gives 2 columns of line numbers
:set autoindent         "continues indentation from previous line
" :set smartindent        "adds indentation sometimes
:set tabstop=2		"a tab is 2 spaces
:set expandtab		"Always use spaces instead of tabs
:set softtabstop=2	"Insert 2 spaces per tab press
:set shiftwidth=2	"An indent is 2 spaces
:set shiftround		"Round indent to nearest shiftwidth multiple
:%retab!
nnoremap <F2> :<C-U>setlocal lcs=tab:>-,trail:-,eol:$ list! list? <CR>

fun! <SID>StripTrailingWhiteSpaces()
	let l = line(".")
	let c = col(".")
	%s/\s\+$//e
	call cursor(l, c)
endfun

autocmd BufWritePre * :call <SID>StripTrailingWhiteSpaces()
" autocmd InsertEnter * :set number
" autocmd InsertLeave * :set relativenumber


" Rainbow Parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
