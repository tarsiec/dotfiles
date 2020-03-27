"         _
"  _   __(_)___ ___  __________
" | | / / / __ `__ \/ ___/ ___/
" | |/ / / / / / / / /  / /__
" |___/_/_/ /_/ /_/_/   \___/  t0maslb@github

let mapleader=" "
call plug#begin('~/.vim/plugged')
Plug 'junegunn/goyo.vim'
Plug 'jreybert/vimagit'
Plug 'preservim/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'fatih/vim-go'
Plug 'plasticboy/vim-markdown'
" colorschemes
Plug 'hzchirs/vim-material'
call plug#end() " Colorscheme
	set t_Co=256
	set termguicolors

	" ayu
		" let ayucolor="dark"
		" colorscheme ayu
	" challenger-deep
		" colorscheme challenger_deep

	" deus
		" set background=dark
		" colorscheme deus
		" let g:deus_termcolors=256

	" rigel
		" colorscheme rigel

	" gruvbox
		" set background=dark
		" colorscheme gruvbox
		" let g:gruvbox_contrast_dark = 'medium'

	" solarized
		" set background=dark
		" colorscheme solarized
	" material ocean
		set background=dark
		let g:material_style='oceanic'
		colorscheme vim-material

" Basics
	set nocompatible
	filetype plugin on
	filetype plugin indent on
	syntax on
	set encoding=utf-8
	set number relativenumber

" Enable autocompletion
	set wildmode=longest,list,full

" set tab size
	set tabstop=4

" Display unprintable chars
	set list
	set listchars=tab:⇒\ ,trail:•,extends:»,precedes:«

" Disable automatic commenting on new line
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Set spellcheck to SP o (orthography) + n (Nglish) or + s (Spanish)
	map <leader>o :setlocal spell! spellang=en_en<CR>
	"TODO alternate with extra key from English to Spanish spellchecking
	" map <leader>o :setlocal spell! spellang=es_es

" Goyo plugin makes text more readable
	map <leader>f :Goyo \| set linebreak<CR>

" Splits at the bottom and right
	set splitbelow splitright

" Shortcutting split navigation
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l

" Check file in spellchecker
	"TODO download and install shellcheck
	map <leader>s :!clear && shellcheck %<CR>

" Compile doc in LaTeX, markdown, groff, etc
	map <leader>c :w! \| !compiler <c-r>%<CR><CR>

" Open corresponding pdf / html / preview
	map <leader>p :!output <c-r>%<CR><CR>

" Ensure files are read as they should
	let g:vimwiki_ext2syntax={'.Rmd': 'markdown', '.rmd': 'markdown', '.md': 'markdown', '.markdown': "markdown", '.mdown': 'markdown'}
	autocmd BufRead,BufNewFile /tmp/calcurse*.,~/.calcurse/notes/* set filetype=markdown
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile *.tex set filetype=tex

" Readmes autowrap text
	autocmd BufRead,BufNewFile *.md set tw=79

" Use urlscan to choose and open an url
	"TODO download and install urlscan
	:noremap <leader>u :w<Home>silent <End> !urlscan<CR>
	:noremap ,, :w<Home>silent <End> !urlscan<CR>

" Airline
	let g:airline_theme='material'
	let g:airline_powerline_fonts=1
	let g:airline#extensions#tabline#enabled=1

" NERDtree
	autocmd StdinReadPre * let s:std_in=1
	autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
	map <leader>n :NERDTreeToggle<CR>
