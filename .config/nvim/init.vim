" NEOVIM CONFIGURATION: THIS TIME WITH STRUCTURE EDITION

call plug#begin("~/.config/nvim/plugins")
	Plug 'tpope/vim-sensible'
    "" LINTING & COMPLETION
	Plug 'autozimu/LanguageClient-neovim', {
				\ 'branch': 'next',
				\ 'do': 'bash install.sh' 
				\ }
	"Plug 'lifepillar/vim-mucomplete'
	"Plug 'roxma/nvim-yarp'
	"Plug 'scrooloose/nerdcommenter'
	Plug 'Shougo/deoplete.nvim'
	Plug 'godlygeek/tabular'
	Plug 'honza/vim-snippets'
	Plug 'sirver/ultisnips'
    
	"" LANGS
	Plug 'fatih/vim-go'
	Plug 'libclang-vim/libclang-vim'
	Plug 'jackguo380/vim-lsp-cxx-highlight'
	Plug 'kovetskiy/sxhkd-vim'
	Plug 'lervag/vimtex'
	Plug 'neovimhaskell/haskell-vim'
	Plug 'plasticboy/vim-markdown'


    
	"" MOTION & SIMPLE TWEAKS
	Plug 'adelarsq/vim-matchit'
	Plug 'christoomey/vim-sort-motion'
	Plug 'christoomey/vim-system-copy'
	Plug 'christoomey/vim-titlecase'
	Plug 'easymotion/vim-easymotion'
	Plug 'haya14busa/incsearch-easymotion.vim'
	Plug 'haya14busa/incsearch.vim'
	Plug 'jiangmiao/auto-pairs'
	Plug 'mattn/emmet-vim'
	Plug 'mg979/vim-visual-multi'
	Plug 'tpope/vim-commentary'
	Plug 'tpope/vim-repeat'
	Plug 'tpope/vim-surround'
	Plug 'vim-scripts/ReplaceWithRegister'
	Plug 'wikitopian/hardmode'

	"" CUSTOM OBJECTS
	" indent
	Plug 'michaeljsmith/vim-indent-object'
	Plug 'kana/vim-textobj-user'
	" inner line | l
	Plug 'kana/vim-textobj-line'
	" closest pair of quotes | q
	Plug 'beloglazov/vim-textobj-quotes'
	" until next puntcuation | u
	Plug 'beloglazov/vim-textobj-punctuation'
	" all doc e
	Plug 'kana/vim-textobj-entire'
	" between params in functions | ,
	Plug 'sgur/vim-textobj-parameter'
	" last pasted text | gb
	Plug 'saaguero/vim-textobj-pastedtext'
	" c/c++ text object | ;
	Plug 'libclang-vim/vim-textobj-clang'
	" between two defined chars | (a/i)f<char>
	Plug 'thinca/vim-textobj-between'
	" innermost brace | j
	Plug 'Julian/vim-textobj-brace'
	" matchit pairs | m
	Plug 'adriaanzon/vim-textobj-matchit'
	
	" IDE-like
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'
    
    "" FILESYSTEM
	Plug 'ggreer/the_silver_searcher'
	Plug 'jreybert/vimagit'
    Plug 'tpope/vim-fugitive' 
    Plug 'airblade/vim-gitgutter'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'preservim/nerdtree' 

    "" AESTHETICAL
	Plug 'dawikur/base16-vim-airline-themes'
	Plug 'dunstontc/vim-vscode-theme'
	Plug 'vim-airline/vim-airline'
    Plug 'chriskempson/base16-vim'
    Plug 'ryanoasis/vim-devicons'
call plug#end()

" for global plugins
let mapleader = " "
" for buffer specific mappings
let maplocalleader = "\\"

set signcolumn=auto
set nocompatible 

""" BASIC FUNCTIONALITY TWEAKS
"" Tabs
set tabstop=4
set shiftwidth=4
set noexpandtab

"" Search
set nohlsearch
set ignorecase
set smartcase

"" Indent
set autoindent
set smartindent

"" Point and click
set mouse=nv

"""" APPEARANCE
filetype plugin indent on
set termguicolors
set background=dark
colorscheme base16-tomorrow-night
set nu rnu
"set cursorline cursorcolumn

set noshowmode
set noshowcmd
set noruler


"let g:lightline = {
            "\ 'colorscheme': 'base16_tomorrow_night'
            "\ }

let g:airline#extensions#tabline#enabled=0
" powerline symbols
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''

let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''

set encoding=utf-8

"TODO CHANGE NAME OF THIS SECTION
"""" MOTION & SHORTCUTS 

""" MAPPINGS
"" TODO Reconsider these
"" Go back to normal mode easily
inoremap jk <esc>

"" move absolute position of lines:
" - up
nnoremap - ddp
" - down
nnoremap _ ddkkp

"" Window
" focus
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
nnoremap <c-k> <c-w>k
nnoremap <c-j> <c-w>j
" size
nnoremap <M-l> :vertical resize +5<cr>
nnoremap <M-h> :vertical resize -5<cr>
nnoremap <M-j> :resize +5<cr>
nnoremap <M-k> :resize -5<cr>

"" Create a terminal
nmap<leader>ot :sp<cr><c-j><M-k><M-k>:terminal<cr>

"" Copy below
nnoremap yu yypk

"" turn a word to uppercase:
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

"" nerdcommenter 
" create default mappings
let g:nerdcreatedefaultmappings=1
" add spaces after comment delimiters by default
let g:nerdspacedelims=1

" Incsearch-Easymotion
nnoremap z/ <Plug>(incsearch-easymotion-/)
nnoremap z? <Plug>(incsearch-easymotion-?)
nnoremap zg/ <Plug>(incsearch-easymotion-stay)



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


augroup filetype_go
	autocmd FileType go :GoAutoTypeInfoToggle
	autocmd FileType go nnoremap <leader>gr :GoRun<cr>
	autocmd FileType go nnoremap <leader>gb :GoBuild<cr>
	autocmd FileType go nnoremap <leader>ge :GoDescribe<cr>
	autocmd FileType go nnoremap <leader>ge :GoDescribe<cr>
	autocmd FileType go nnoremap <leader>go :GoDoc<cr>
	autocmd FileType go nnoremap <leader>gob :GoDocBrowser<cr>
	" change
	autocmd FileType go nnoremap <leader>gn :GoRename<cr>
	" goto
	autocmd FileType go nnoremap <leader>gp :GoPointsTo<cr>
	autocmd FileType go nnoremap <leader>gfr :GoReferrers<cr>
	" calls
	autocmd FileType go nnoremap <leader>gfc :GoCallers<cr>
	autocmd FileType go nnoremap <leader>gfe :GoCallees<cr>
	" tests
	autocmd FileType go nnoremap <leader>gt :GoTest<cr>
	autocmd FileType go nnoremap <leader>gtf :GoTestFunc<cr>
	autocmd FileType go nnoremap <leader>gtc :GoTestCompile<cr>
	" debug
	autocmd FileType go nnoremap <leader>gds :GoDebugStart<cr>
	autocmd FileType go nnoremap <leader>gdt :GoDebugTestFunc<cr>
	autocmd FileType go nnoremap <leader>gdb :GoDebugBreakpoint<cr>
	autocmd FileType go nnoremap <leader>gdc :GoDebugContinue<cr>
	autocmd FileType go nnoremap <leader>gdq :GoDebugStop<cr>
augroup END


autocmd BufEnter :call HardMode()<cr>
nnoremap <silent> <leader>sh :call ToggleHardMode()<cr>
nnoremap <leader>se :call EasyMode()<cr>


"""" FILES, PROJECTS & VERSION MANAGEMENT
"" Git stuff
nnoremap <leader>gf :GFiles<cr>
nnoremap <leader>gs :GFiles?<cr>
nnoremap <leader>gc :Commits<cr>
nnoremap <leader>gb :BCommits<cr>

nnoremap <leader>gm :Magit<cr>

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
  let $FZF_DEFAULT_COMMAND='rg --files --follow --no-ignore-vcs --hidden -g "!{**/node_modules/**,.git/*,**/*.pem}"'
  let $FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
  "let $FZF_DEFAULT_OPTS="--ansi --preview-window 'right:60%' --layout reverse --margin=1,4 --preview 'bat --color=always --style=header,grid --line-range :300 {}'"



nnoremap zf :Files<cr>
nnoremap zc :Colors<cr>
nnoremap zb :Buffers<cr>
nnoremap zr :Rg 
nnoremap zla :Lines<cr>
nnoremap zlc :Lines<cr>
nnoremap zta :Tags<cr>
nnoremap ztc :BTags<cr>
nnoremap zm :Marks<cr>
nnoremap zw :Windows<cr>
nnoremap zo :Locate 
nnoremap zp :History<cr>
nnoremap zi :History/<cr>
nnoremap zn :Snippets<cr>
nnoremap zhc :Commands<cr>
nnoremap zhc :Maps<cr>
nnoremap zht :Helptags<cr>
nnoremap zt :Filetypes<cr>


"""" LANGUAGE SERVER
set hidden

let g:LanguageClient_serverCommands={
			\ 'c': ['clangd', '--fallback-style=webkit'],
			\ 'cpp': ['clangd'],
			\ 'go': ['gopls'],
			\ 'haskell': ['haskell-language-server-wrapper', '--lsp'],
			\ 'python': ['pyls'],
			\ 'rust': ['rls']
			\ }


nnoremap <silent> gh :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> gr :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> gu :call LanguageClient#textDocument_references()<CR>
nnoremap <silent> gm :call LanguageClient_contextMenu()<CR>

" nnoremap <silent> <leader>fd :call LanguageClient#textDocument_formatting()<CR>
" nnoremap <silent> <leader>gl :call LanguageClient#textDocument_codeLens()<CR>

" Completion
let g:deoplete#enable_at_startup=1
call deoplete#custom#source('LanguageClient',
			\ 'min_pattern_length',
			\ 2)
set completeopt+=menuone
set completeopt+=noselect
set shortmess+=c
set belloff+=ctrlg
set completeopt-=preview

" ultisnips
let g:UltiSnipsForwardTrigger="<c-n>"


"""" LANGUAGE SERVER
"" Vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
"" TODO Reconsider
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'
