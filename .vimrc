" Pathogen load
filetype off

call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on
syntax on

" Disable all cursor blinking:
:set guicursor+=a:blinkon0
" Default setting, uncommong to restore:
" :set guicursor&

" tab stuffs
:set number
:set relativenumber
:set tabstop=4
:set noexpandtab
:%retab!
nnoremap <F2> :<C-U>setlocal lcs=tab:>-,trail:-,eol:$ list! list? <CR>

fun! <SID>StripTrailingWhiteSpaces()
	let l = line(".")
	let c = col(".")
	%s/\s\+$//e
	call cursor(l, c)
endfun

autocmd BufWritePre * :call <SID>StripTrailingWhiteSpaces()
autocmd InsertEnter * :set number
autocmd InsertLeave * :set relativenumber

