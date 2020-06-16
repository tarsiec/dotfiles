"         _
"  _   __(_)___ ___  __________
" | | / / / __ `__ \/ ___/ ___/
" | |/ / / / / / / / /  / /__
" |___/_/_/ /_/ /_/_/   \___/  t0maslb@github

let mapleader=" "
call plug#begin('~/.vim/plugged')
Plug 'jreybert/vimagit'
Plug 'fatih/vim-go'
Plug 'sheerun/vim-polyglot'
Plug 'pangloss/vim-javascript'
" ide-ing
Plug 'davidhalter/jedi-vim'
Plug 'Shougo/deoplete.nvim'
Plug 'roxma/nvim-yarp'
Plug 'preservim/nerdtree'
Plug 'itchyny/lightline.vim'
" Plug 'itchyny/lightline.vim'
Plug 'junegunn/goyo.vim'
" ncm2
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
" colorschemes
Plug 'chriskempson/base16-vim'

call plug#end() 

set t_Co=256
set termguicolors
let base16colorspace=256

set background=dark
syntax enable
colorscheme base16-tomorrow-night

" Basics
set nocompatible
filetype plugin on
filetype plugin indent on

set encoding=utf-8
set number relativenumber

" Enable autocompletion
set wildmode=longest,list,full

" set tab size
set tabstop=4

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

" NERDtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
	map <leader>n :NERDTreeToggle<CR>

" airline
"let g:airline#extensions#tabline#enabled=1
"let g:airline#extensions#tabline#formatter='default'
"let g:airline_powerline_fonts=1
"let g:airline_theme='dracula'

" lightline
let g:lightline = {
\ 'colorscheme': 'Tomorrow_Night',
\ }

" ncm2
" suppress the annoying 'match x of y', 'The only match' and 'Pattern not
" found' messages
set shortmess+=c

" CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
inoremap <c-c> <ESC>

" When the <Enter> key is pressed while the popup menu is visible, it only
" hides the menu. Use this mapping to close the menu and also start a new
" line.
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
