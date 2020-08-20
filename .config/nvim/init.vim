"         _
"  _   __(_)___ ___  __________
" | | / / / __ `__ \/ ___/ ___/
" | |/ / / / / / / / /  / /__
" |___/_/_/ /_/ /_/_/   \___/  t0maslb@github

let mapleader=" "

call plug#begin('~/.vim/plugged')
    Plug 'jreybert/vimagit'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'junegunn/goyo.vim'
    Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
    Plug 'sirver/ultisnips'
    Plug 'airblade/vim-gitgutter'
    Plug 'terryma/vim-multiple-cursors'
    Plug 'ryanoasis/vim-devicons'
    Plug 'vimwiki/vimwiki'
    Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }
    " langs
    Plug 'sheerun/vim-polyglot'
    Plug 'rust-lang/rust.vim'
    Plug 'lervag/vimtex'
    Plug 'LaTeX-Box-Team/LaTeX-Box'
    " IDEing
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'preservim/nerdtree'
    Plug 'ctrlpvim/ctrlp.vim'
"    Plug 'ycm-core/YouCompleteMe'
    Plug 'vim-crystal/vim-crystal'
    Plug 'vimlab/split-term.vim'
    Plug 'jiangmiao/auto-pairs'
    Plug 'voldikss/vim-floaterm'
    " colorschemes
    Plug 'lifepillar/vim-solarized8'
    Plug 'altercation/vim-colors-solarized'
    Plug 'vim-airline/vim-airline-themes'
call plug#end()


"--- APPEARANCE ---
set t_Co=256
set termguicolors
set background=dark
syntax enable
colorscheme solarized8_flat
hi Normal guibg=NONE ctermbg=NONE


"--- MISC ---
filetype plugin indent on
"set mouse=a
set encoding=utf-8
set number relativenumber
set nohls
" Tab settings
" set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
" lines
"set cursorline
"set cursorcolumn
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
map <leader>tf :FloatermNew <CR>

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

"--- CODE GEN  ---
" Guide navigation
noremap <leader>m <Esc>/<++><Enter>"_c4l
inoremap <A-TAB>m <Esc>/<++><Enter>"_c4l
vnoremap <leader>m <Esc>/<++><Enter>"_c4l

" General insert commands
inoremap <A-TAB>; <++>

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
" COC.nvim
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>



" airline
let g:airline#extensions#tabline#enabled=1
let g:airline#extensions#tabline#formatter='default'
let g:airline_powerline_fonts=1
let g:airline_theme='solarized'

" lightline
"let g:lightline = {
"\ 'colorscheme': 'Tomorrow_Night',
"\ }

" hexokinase
let g:Hexokinase_highlighters = ['backgroundfull']

" ultisnips
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

" --- LATEX ---
" Vim Latex Live Preview
let g:livepreview_previewer = 'zathura'

noremap <leader>p :LLPStartPreview<CR>
inoremap <A-TAB>p <ESC>:LLPStartPreview<CR>i
vnoremap <A-TAB>p <ESC>:LLPStartPreview<CR>i

" vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'

" YouCompleteMe
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>d  :YcmCompleter GoToDefinitionElseDeclaration<CR>


" Split-Term
map <leader>ts :10Term<CR>
map <leader>tv :VTerm<CR>
map <leader>tt :TTerm<CR>

