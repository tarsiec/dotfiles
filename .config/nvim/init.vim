" NEOVIM CONFIGURATION: THIS TIME WITH STRUCTURE EDITION

call plug#begin("~/.config/nvim/plugins")
    "" LINTING & COMPLETION
	Plug 'autozimu/LanguageClient-neovim', {
				\ 'branch': 'next',
				\ 'do': 'bash install.sh' 
				\ }
	Plug 'Shougo/deoplete.nvim'
	Plug 'roxma/nvim-yarp'
	Plug 'godlygeek/tabular'
	" Plug 'sirver/ultisnips'
	" Plug 'honza/vim-snippets'
	Plug 'scrooloose/nerdcommenter'
    
	"" LANGS
	Plug 'lervag/vimtex'
	Plug 'fatih/vim-go'
	Plug 'plasticboy/vim-markdown'
	Plug 'jackguo380/vim-lsp-cxx-highlight'
    
	"" MOTION & SIMPLE TWEAKS
	Plug 'jiangmiao/auto-pairs'
	Plug 'mattn/emmet-vim'
	Plug 'mg979/vim-visual-multi'
	Plug 'tpope/vim-surround'
	Plug 'easymotion/vim-easymotion'
	Plug 'haya14busa/incsearch.vim'
	Plug 'haya14busa/incsearch-easymotion.vim'

	"" IDE-like
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'

    
    "" FILESYSTEM
    Plug 'preservim/nerdtree' 
    Plug 'ctrlpvim/ctrlp.vim' 
    Plug 'tpope/vim-fugitive' 
    Plug 'airblade/vim-gitgutter'
	Plug 'ggreer/the_silver_searcher'

    "" AESTHETICAL
    Plug 'chriskempson/base16-vim'
    Plug 'arcticicestudio/nord-vim'
    Plug 'ryanoasis/vim-devicons'
    Plug 'itchyny/lightline.vim'
    Plug 'mike-hearn/base16-vim-lightline'
call plug#end()

" for global plugins
let mapleader = " "
" for buffer specific mappings
let maplocalleader = "\\"

set signcolumn=auto

""" BASIC FUNCTIONALITY TWEAKS
"" Tabs
set tabstop=4
set shiftwidth=4

"" Search
set nohlsearch

"" Indent
set autoindent
set smartindent

"" Point and click
set mouse=nv



"""" APPEARANCE
filetype plugin indent on
set termguicolors
colorscheme base16-tomorrow-night
set nu rnu
set cursorline

set noshowmode
set noshowcmd
set noruler

let g:lightline = {
            \ 'colorscheme': 'base16_tomorrow_night'
            \ }


"TODO CHANGE NAME OF THIS SECTION
"""" MOTION & SHORTCUTS 

""" MAPPINGS
"" TODO Reconsider these
"" Go back to normal mode easily
inoremap jk <esc>

"" TODO Remember this is maybe temporary
"" Force myself to not have bad habits in Vim
" - no esc
inoremap    <esc>   <nop>
" - no arrow keys
noremap   <left>    <nop>
noremap   <right>   <nop>
noremap   <up>      <nop>
noremap   <down>    <nop>

"" move absolute position of lines:
" - up
nnoremap - ddp
" - down
nnoremap _ ddkkp

"" turn a word to uppercase:
" - in normal mode
nnoremap <c-u> viwu<esc>e
" - in insert mode
inoremap <c-u> <esc>viwu<esc>eli

"" surround words:
" - in quotes
nnoremap <leader>i" viw<esc>a"<esc>bi"<esc>lel
vnoremap <leader>i" <esc>`<i"<esc>`>la"<esc>l

"" edit vimrc as split
nnoremap <leader>ev :split $MYVIMRC<cr>

"" source the vimrc file wherever i am
nnoremap <leader>sv :source $MYVIMRC<cr>

"" todo remove these maybe
"" go to [x] of the line
" - beginning
" nnoremap h 0
" - end
" nnoremap l $

"" operator-pending mappings
" - inside next/prev parens
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! f)vi(<cr>
" - inside next/prev angle brackets
onoremap in< :<c-u>normal! f<vi<<cr>
onoremap il< :<c-u>normal! f>vi<<cr>
" - inside next/prev curly brackets
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap il{ :<c-u>normal! f}vi{<cr>
" - inside next/prev parens
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap il" :<c-u>normal! f"vi"<cr>

"" nerd commenter 
" create default mappings
let g:nerdcreatedefaultmappings=1
" add spaces after comment delimiters by default
let g:nerdspacedelims=1

" Incsearch-Easymotion
map z/ <Plug>(incsearch-easymotion-/)
map z? <Plug>(incsearch-easymotion-?)
map zg/ <Plug>(incsearch-easymotion-stay)



""" abbreviations
"" abbreviate *noremap <leader>
iabbrev nnr nnoremap
iabbrev inr inoremap
iabbrev vnr vnoremap 

"" abbreviate the abbreviation input
iabbrev ibv iabbrev

"" some personal ones
iabbrev @name@ tomás lópez brea
iabbrev @email@ tomaslb@tutanota.com
iabbrev @website@ https://tarsiec.com
iabbrev @gh@ https://github.com/tarsiec

"" todo remove these
"" add quick if's
augroup add_if
	autocmd!
	" - in python
	autocmd filetype python		:iabbrev <buffer> iff if:<left>
	" - in javascript
	autocmd filetype javascript	:iabbrev <buffer> iff if ()<left>
augroup end

"i" add return statements
augroup add_return
	autocmd!
	autocmd filetype python		iabbrev rtn return
	autocmd filetype javascript 	iabbrev rtn return
	autocmd filetype golang		iabbrev rtn return
augroup end

augroup enter_function
	autocmd!
	" - in python
	autocmd filetype python		:iabbrev <buffer> fnc def ():<left><left><left>
	autocmd filetype python		:iabbrev <buffer> def nope
	" - in javascript
	autocmd filetype javascript	:iabbrev <buffer> fnc function(){}<left><left><left><left>
augroup end

""" autocommands
"" add comments to the line
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
"" Git stuff
nnoremap <leader>gf :GFiles<cr>
nnoremap <leader>gs :GFiles?<cr>
nnoremap <leader>gc :Commits<cr>
nnoremap <leader>gc :BCommits<cr>

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

"" FZF
nnoremap <leader>zf :Files<cr>
nnoremap <leader>zc :Colors<cr>
nnoremap <leader>zb :Buffers<cr>
nnoremap <leader>zr :Rg 
nnoremap <leader>zla :Lines<cr>
nnoremap <leader>zlc :Lines<cr>
nnoremap <leader>zta :Tags<cr>
nnoremap <leader>ztc :BTags<cr>
nnoremap <leader>zm :Marks<cr>
nnoremap <leader>zw :Windows<cr>
nnoremap <leader>zo :Locate 
nnoremap <leader>zp :History<cr>
nnoremap <leader>z/ :History/<cr>
nnoremap <leader>zn :Snippets<cr>
nnoremap <leader>zhc :Commands<cr>
nnoremap <leader>zhc :Maps<cr>
nnoremap <leader>zht :Helptags<cr>
nnoremap <leader>zt :Filetypes<cr>


"""" LANGUAGE SERVER
set hidden

let g:LanguageClient_serverCommands={
			\ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
			\ 'c': ['clangd'],
			\ 'go': ['gopls']
			\ }


nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> <leader>fd :call LanguageClient#textDocument_formatting()<CR>


let g:deoplete#enable_at_startup=1


"""" LANGUAGE SERVER
"" Vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
"" TODO Reconsider
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'
