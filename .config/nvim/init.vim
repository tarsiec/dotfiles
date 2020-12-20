" NEOVIM CONFIGURATION: THIS TIME WITH STRUCTURE EDITION

call plug#begin("~/.config/nvim/plugins")

"" Motion
Plug 'jiangmiao/auto-pairs'


"" Filesys
" NERDTree
Plug 'preservim/nerdtree'

" Ctrl-P
Plug 'ctrlpvim/ctrlp.vim'

"" Aesthetical
" Base16 for vim
Plug 'chriskempson/base16-vim'

" Devicons
Plug 'ryanoasis/vim-devicons'

call plug#end()

" for global plugins
let mapleader = " "
" for buffer specific mappings
let maplocalleader = "\\"



""" BASIC FUNCTIONALITY TWEAKS
"" Tabs
set tabstop=4
set shiftwidth=4
set expandtab

"" Search
set nohlsearch

"" Indent
set autoindent
set smartindent



"""" APPEARANCE
set termguicolors
colorscheme base16-tomorrow-night
set nu rnu



"TODO CHANGE NAME OF THIS SECTION
"""" MOTION & SHORTCUTS 

""" MAPPINGS
"" TODO Reconsider these
"" Go back to normal mode easily
inoremap jk <esc>


"" TODO Remember this is maybe temporary
"" Force myself to not have bad habits in Vim
" - no esc
inoremap <esc> <nop>
" - no arrow keys
nnoremap <left> <nop>
inoremap <left> <nop>
vnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <right> <nop>
vnoremap <right> <nop>
nnoremap <up> <nop>
inoremap <up> <nop>
vnoremap <up> <nop>
nnoremap <down> <nop>
inoremap <down> <nop>
vnoremap <down> <nop>

"" Move absolute position of lines:
" - up
nnoremap - ddp
" - down
nnoremap _ ddkkp

"" Turn a word to uppercase:
" - in normal mode
nnoremap <c-u> viwU<esc>e
" - in insert mode
inoremap <c-u> <esc>viwU<esc>eli

"" Surround words:
" - in quotes
nnoremap <leader>i" viw<esc>a"<esc>bi"<esc>lel 
vnoremap <leader>i" <esc>`<i"<esc>`>la"<esc>l

"" Edit vimrc as split
nnoremap <leader>ev :split $MYVIMRC<cr>

"" Source the vimrc file wherever I am
nnoremap <leader>sv :source $MYVIMRC<cr>

"" TODO Remove these maybe
"" Go to [x] of the line
" - beginning
" nnoremap H 0
" - end
" nnoremap L $

"" Operator-pending mappings
" - inside next/prev parens
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>
" - inside next/prev angle brackets
onoremap in< :<c-u>normal! f<vi<<cr>
onoremap il< :<c-u>normal! F>vi<<cr>
" - inside next/prev curly brackets
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap il{ :<c-u>normal! F}vi{<cr>
" - inside next/prev parens
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap il" :<c-u>normal! F"vi"<cr>


""" ABBREVIATIONS
"" Abbreviate *noremap <leader>
iabbrev nnr nnoremap
iabbrev inr inoremap
iabbrev vnr vnoremap 

"" Abbreviate the abbreviation input
iabbrev ibv iabbrev

"" Some personal ones
iabbrev @name@ Tomás López Brea
iabbrev @email@ tomaslb@tutanota.com
iabbrev @website@ https://gsae.es
iabbrev @gh@ https://github.com/tarsiec

"" TODO Remove these
"" Add quick if's
augroup add_if
	autocmd!
	" - in python
	autocmd FileType python		:iabbrev <buffer> iff if:<left>
	" - in javascript
	autocmd FileType javascript	:iabbrev <buffer> iff if ()<left>
augroup END

"I" Add return statements
augroup add_return
	autocmd!
	autocmd FileType python		iabbrev rtn return
	autocmd FileType javascript 	iabbrev rtn return
	autocmd FileType golang		iabbrev rtn return
augroup END

augroup enter_function
	autocmd!
	" - in python
	autocmd FileType python		:iabbrev <buffer> fnc def ():<left><left><left>
	autocmd FileType python		:iabbrev <buffer> def nope
	" - in javascript
	autocmd FileType javascript	:iabbrev <buffer> fnc function(){}<left><left><left><left>
augroup END

""" AUTOCOMMANDS
"" Add comments to the line
augroup comment_line
	autocmd!
	" - in python
	autocmd FileType python		nnoremap <buffer> <localleader>c I#<esc>
	" - in // langs
	autocmd FileType javascript	nnoremap <buffer> <localleader>c I//<esc>
	autocmd FileType golang		nnoremap <buffer> <localleader>c I//<esc>
augroup END

augroup filetype_html
	autocmd!
	autocmd FileType html		nnoremap <buffer> <localleader>f Vatzf
augroup END



"""" FILES, PROJECTS & VERSION MANAGEMENT
"" NERDTree config
" remaps
nnoremap <leader>of :NERDTreeToggle<cr>
nnoremap <leader>ff :NERDTreeFocus<cr>
nnoremap <leader>sf :NERDTreeFind<cr>

"" CtrlP config
" remaps
nnoremap <leader>op :CtrlP<cr>
nnoremap <leader>ob :CtrlPBuffer<cr>
inoremap <c-p> <esc>:CtrlP<cr>


"""" SYNTAX HILIGHTING



"""" LANGUAGE SERVER



