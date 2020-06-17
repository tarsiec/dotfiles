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
    Plug 'roxma/nvim-yarp'
    Plug 'preservim/nerdtree'
    "Plug 'itchyny/lightline.vim'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'junegunn/goyo.vim'
    Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
    Plug 'voldikss/vim-floaterm'
    Plug 'airblade/vim-gitgutter'
    Plug 'terryma/vim-multiple-cursors'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'ryanoasis/vim-devicons'
    Plug 'jceb/vim-orgmode'
    " colorschemes
    Plug 'chriskempson/base16-vim'
call plug#end()


"--- APPEARANCE ---
set t_Co=256
set termguicolors
let base16colorspace=256
set background=dark
syntax enable
colorscheme base16-tomorrow-night
hi Normal guibg=NONE ctermbg=NONE


"--- MISC ---
set nocompatible
filetype plugin on
filetype plugin indent on
set mouse=a
set clipboard+=unnamedplus
set encoding=utf-8
set number relativenumber
set nohls
" Tab settings
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
" lines
set cursorline
set cursorcolumn
" ignore cases when searching except when used
set ignorecase
set smartcase
" detect tex files
autocmd BufRead,BufNewFile *.tex set filetype=tex
" vert center doc when in insert mode
autocmd InsertEnter * norm zz
" remove trailing whitesave on save
autocmd BufWritePre * %s/\s\+$//e
" Enable autocompletion
set wildmode=longest,list,full
" show indent
" Splits at the bottom and right
set splitbelow splitright

" --- MAPPINGS ---
" Set spellcheck to SP o (orthography) + n (Nglish) or + s (Spanish)
map <leader>on :setlocal spell! spelllang=en<CR>
map <leader>os :setlocal spell! spelllang=es<CR>

" Goyo plugin makes text more readable
map <leader>g :Goyo \| set linebreak<CR>

" Shortcutting split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Tab shortcuts
nnoremap <A-h> :tabp<CR>
nnoremap <A-l> :tabn<CR>

" CtrlP
let g:ctrlp_map = '<c-p>'

" Check file in spellchecker
map <leader>s :!clear && shellcheck %<CR>

" Compile doc in LaTeX, markdown, groff, etc
map <leader>c :w! \| !compiler <c-r>%<CR><CR>

" Open corresponding pdf / html / preview
map <leader>p :!output <c-r>%<CR><CR>

" Float Term
map <leader>t :FloatermNew <CR>

" enable and disable auto comment
map <leader>c :setlocal formatoptions-=cro<CR>
map <leader>C :setlocal formatoptions=cro<CR>

" Use urlscan to choose and open an url
"TODO download and install urlscan
:noremap <leader>u :w<Home>silent <End> !urlscan<CR>
:noremap ,, :w<Home>silent <End> !urlscan<CR>

" NERDtree
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
map <leader>n :NERDTreeToggle<CR>
inoremap <A-TAB>] <ESC>:NERDTreeToggle<CR>
noremap <A-TAB>] :NERDTreeToggle<CR>

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" shortcut to replace all insatnces to S
nnoremap S :%s//gI<Left><Left><Left>


" --- CODE GEN  ---
" Guide navigation
noremap <leader>n <Esc>/<++><Enter>"_c4l
inoremap <A-TAB>n <Esc>/<++><Enter>"_c4l
vnoremap <leader>n <Esc>/<++><Enter>"_c4l

" General insert commands
inoremap ;m <++>

" shell
map <leader>b i#!/bin/sh<CR><CR>
autocmd FileType sh inoremap <A-TAB>f ()<Space>{<CR><Tab><++><CR>}<CR><CR><++><Esc>?()<CR>
autocmd FileType sh inoremap <A-TAB>i if<Space>[<Space>];<Space>then<CR><++><CR>fi<CR><CR><++><Esc>?];<CR>hi<Space>
autocmd FileType sh inoremap <A-TAB>ei elif<Space>[<Space>];<Space>then<CR><++><CR><Esc>?];<CR>hi<Space>
autocmd FileType sh inoremap <A-TAB>sw case<Space>""<Space>in<CR><++>)<Space><++><Space>;;<CR><++><CR>esac<CR><CR><++><Esc>?"<CR>i
autocmd FileType sh inoremap <A-TAB>ca )<Space><++><Space>;;<CR><++><Esc>?)<CR>i
" markdown
autocmd FileType markdown noremap <A-TAB>r i---<CR>title:<Space><++><CR>author:<Space>"Brodie Robertson"<CR>geometry:<CR>-<Space>top=30mm<CR>-<Space>left=20mm<CR>-<Space>right=20mm<CR>-<Space>bottom=30mm<CR>header-includes:<Space>\|<CR><Tab>\usepackage{float}<CR>\let\origfigure\figure<CR>\let\endorigfigure\endfigure<CR>\renewenvironment{figure}[1][2]<Space>{<CR><Tab>\expandafter\origfigure\expandafter[H]<CR><BS>}<Space>{<CR><Tab>\endorigfigure<CR><BS>}<CR><BS>---<CR><CR>
autocmd FileType markdown inoremap <A-TAB>i ![](<++>){#fig:<++>}<Space><CR><CR><++><Esc>kkF]i
autocmd FileType markdown inoremap <A-TAB>a [](<++>)<Space><++><Esc>F]i
autocmd FileType markdown inoremap <A-TAB>1 #<Space><CR><CR><++><Esc>2k<S-a>
autocmd FileType markdown inoremap <A-TAB>2 ##<Space><CR><CR><++><Esc>2k<S-a>
autocmd FileType markdown inoremap <A-TAB>3 ###<Space><CR><CR><++><Esc>2k<S-a>
autocmd FileType markdown inoremap <A-TAB>4 ####<Space><CR><CR><++><Esc>2k<S-a>
autocmd FileType markdown inoremap <A-TAB>5 #####<Space><CR><CR><++><Esc>2k<S-a>
autocmd FileType markdown inoremap <A-TAB>u +<Space><CR><++><Esc>1k<S-a>
autocmd FileType markdown inoremap <A-TAB>o 1.<Space><CR><++><Esc>1k<S-a>
autocmd FileType markdown inoremap <A-TAB>f +@fig:


"--- EXTENSION SETTINGS ---
" airline
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#formatter='default'
let g:airline_powerline_fonts=1
let g:airline_theme='base16_tomorrow'

" lightline
"let g:lightline = {
"\ 'colorscheme': 'Tomorrow_Night',
"\ }

" hexokinase
let g:Hexokinase_highlighters = ['backgroundfull']
<<<<<<< HEAD
noremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
=======
>>>>>>> script
