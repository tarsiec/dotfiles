" NEOVIM CONFIGURATION: THIS TIME WITH STRUCTURE EDITION

call plug#begin("~/.config/nvim/plugins")
	Plug 'tpope/vim-sensible'
	" Plug 'jaxbot/browserlink.vim'



    "" LINTING & COMPLETION
	Plug 'godlygeek/tabular'
	Plug 'honza/vim-snippets'
	Plug 'sirver/ultisnips'
	Plug 'junegunn/goyo.vim'
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
	Plug 'liuchengxu/vim-which-key'

	
    
	"" LANGS
	Plug 'kovetskiy/sxhkd-vim'
	Plug 'lervag/vimtex'
	Plug 'plasticboy/vim-markdown'
	Plug 'tmhedberg/SimpylFold'
	Plug 'neovimhaskell/haskell-vim'
	Plug 'vimwiki/vimwiki'


	""" CTAGS
	Plug 'universal-ctags/ctags'
	Plug 'ludovicchabant/vim-gutentags'


    
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
	Plug 'voldikss/vim-floaterm'



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
	" Plug 'thinca/vim-textobj-between'
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
	Plug 'itchyny/lightline.vim'
	Plug 'Lokaltog/vim-distinguished'
	Plug 'embark-theme/vim'
	Plug 'w0ng/vim-hybrid'
	Plug 'embark-theme/vim'
	Plug 'daviesjamie/vim-base16-lightline'
	Plug 'pineapplegiant/spaceduck'
	Plug 'dunstontc/vim-vscode-theme'
	Plug 'altercation/vim-colors-solarized'
	Plug 'herrbischoff/cobalt2.vim'
	Plug 'morhetz/gruvbox'
	Plug 'tomasr/molokai'
	Plug 'joshdick/onedark.vim'
	Plug 'jnurmine/zenburn'
	Plug 'dracula/vim'
	Plug 'ayu-theme/ayu-vim'
	Plug 'junegunn/seoul256.vim'
	Plug 'nanotech/jellybeans.vim'
	Plug 'arzg/vim-colors-xcode'
	Plug 'arcticicestudio/nord-vim'
    Plug 'chriskempson/base16-vim'
    Plug 'ryanoasis/vim-devicons'
	Plug 'phanviet/vim-monokai-pro'
call plug#end()

" for global plugins
let mapleader = " "
" for buffer specific mappings
let maplocalleader = "\\"

set signcolumn=auto
set nocompatible 
set splitbelow
set splitright
set timeoutlen=500
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>
" set cursorline

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
set mouse=nvi

"" Code folding
set foldmethod=indent
set foldlevel=99

nnoremap <c-i> za

"""" APPEARANCE
set t_Co=256
filetype plugin indent on
syntax enable
set termguicolors
set background=dark
" colorscheme hybrid
colorscheme base16-seti
" highlight Normal ctermbg=NONE guibg=NONE
" highlight LineNr ctermbg=NONE guibg=NONE
" highlight SignColumn ctermbg=NONE guibg=NONE
" highlight Column ctermbg=NONE guibg=NONE
" highlight CursorLine ctermbg=NONE guibg=NONE
" colorscheme nord
" colorscheme solarized
set nu rnu
set cursorline

set noshowmode
set noshowcmd
set noruler

set laststatus=2


" let g:base16lightline_hcontrast=1
let g:lightline = { 'colorscheme': 'base16' }

" let g:airline#extensions#tabline#enabled=0
" powerline symbols
" let g:airline#extensions#tabline#left_sep = ''
" let g:airline#extensions#tabline#left_alt_sep = ''

" let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
" let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''

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
nmap<leader>oT :sp<cr><c-j><M-k><M-k>:terminal<cr>
nmap<leader>ot :FloatermToggle<cr>

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
" - inside next/prev square brackets
onoremap in[ :<c-u>normal! f[vi{<cr>
onoremap il[ :<c-u>normal! f]vi{<cr>
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

"" Writing
" toggle write mode
nnoremap <leader>tw :Goyo<cr>


" Incsearch-Easymotion
map <leader>e <Plug>(easymotion-prefix)
" es, egE

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
	autocmd filetype python		:iabbrev <buffer> fnc def():<left><left><left>
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
	autocmd FileType html set tabstop=2
 	autocmd FileType html set shiftwidth=2
	autocmd FileType html	nnoremap <buffer> <localleader>f Vatzf
augroup END

augroup filetype_c
	autocmd!
	autocmd FileType c		nnoremap <silent> <leader>cc :FloatermNew --autoclose=0 gcc % -o %< && ./%<<cr>
augroup END

augroup filetype_rust
	autocmd!
	autocmd FileType rust	nnoremap <silent> <leader>cc :FloatermNew --autoclose=0 cargo run<cr>
augroup END

" augroup filetype_python
" 	autocmd FileType python
" 		" change
" 		\ let g:jedi#rename_command = "<leader>cr"
" 		\ let g:SimpylFold_docstring_preview=1
" 		\ let g:jedi#auto_initalization
" 		" goto
" 		\ let g:jedi#goto_command = "<leader>cg"
" 		\ let g:jedi#goto_assignment_command = "<leader>cd"
" 		" calls
" 		" tests
" 		" debug
" augroup END

augroup filetype_go
	autocmd FileType go		nnoremap <silent> <leader>cc :FloatermNew --autoclose=0 go run %<cr>
" 	autocmd FileType go :GoAutoTypeInfoToggle
" 	autocmd FileType go nnoremap <leader>cr :GoRun<cr>
" 	autocmd FileType go nnoremap <leader>cb :GoBuild<cr>
" 	autocmd FileType go nnoremap <leader>ce :GoDescribe<cr>
" 	autocmd FileType go nnoremap <leader>ce :GoDescribe<cr>
" 	autocmd FileType go nnoremap <leader>co :GoDoc<cr>
" 	autocmd FileType go nnoremap <leader>cob :GoDocBrowser<cr>
" 	" change
" 	autocmd FileType go nnoremap <leader>cn :GoRename<cr>
" 	" goto
" 	autocmd FileType go nnoremap <leader>cp :GoPointsTo<cr>
" 	autocmd FileType go nnoremap <leader>cfr :GoReferrers<cr>
" 	" calls
" 	autocmd FileType go nnoremap <leader>cfc :GoCallers<cr>
" 	autocmd FileType go nnoremap <leader>cfe :GoCallees<cr>
" 	" tests
" 	autocmd FileType go nnoremap <leader>ct :GoTest<cr>
" 	autocmd FileType go nnoremap <leader>ctf :GoTestFunc<cr>
" 	autocmd FileType go nnoremap <leader>ctc :GoTestCompile<cr>
" 	" debug
" 	autocmd FileType go nnoremap <leader>cds :GoDebugStart<cr>
" 	autocmd FileType go nnoremap <leader>cdt :GoDebugTestFunc<cr>
" 	autocmd FileType go nnoremap <leader>cdb :GoDebugBreakpoint<cr>
" 	autocmd FileType go nnoremap <leader>cdc :GoDebugContinue<cr>
" 	autocmd FileType go nnoremap <leader>cdq :GoDebugStop<cr>
augroup END


augroup filetype_tex
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cc :VimtexCompile<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>ct :VimtexTocToggle<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cT :VimtexTocOpen<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cq :VimtexStop<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cQ :VimtexStopAll<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cv :VimtexView<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cw :VimtexCountWords<cr>
	autocmd BufRead,BufNew *.tex nnoremap <localleader>cl :VimtexCountLetters<cr>
augroup END


autocmd BufEnter :call HardMode()<cr>
nnoremap <silent> <leader>th :call ToggleHardMode()<cr>


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



nnoremap <leader>zf :Files<cr>
nnoremap <leader>zc :Colors<cr>
nnoremap <leader>zb :Buffers<cr>
nnoremap <leader>zr :Rg 
nnoremap <leader>zs :Lines<cr>
nnoremap <leader>zls :BLines<cr>
nnoremap <leader>zt :Tags<cr>
nnoremap <leader>zlt :BTags<cr>
nnoremap <leader>zm :Marks<cr>
nnoremap <leader>zw :Windows<cr>
nnoremap <leader>zo :Locate 
nnoremap <leader>zp :History<cr>
nnoremap <leader>zi :History/<cr>
nnoremap <leader>zn :Snippets<cr>
snoremap <leader>zhc :Commands<cr>
nnoremap <leader>zhc :Maps<cr>
nnoremap <leader>zht :Helptags<cr>
nnoremap <leader>zt :Filetypes<cr>


"""" LANGUAGE SERVER
set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif


" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nmap <silent> <leader>cd <Plug>(coc-definition)
nmap <silent> <leader>cy <Plug>(coc-type-definition)
nmap <silent> <leader>ci <Plug>(coc-implementation)
nmap <silent> <leader>cu <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>cr <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>cF  <Plug>(coc-format-selected)
nmap <leader>cF  <Plug>(coc-format-selected)
nmap <leader>cf  :Format<cr>:%retab!<cr>
autocmd FileType haskell
			\ unmap <leader>cf
			\ nmap <leader>cf  :Format<cr>:%retab!<cr>

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end


" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>ca  <Plug>(coc-codeaction-selected)
nmap <leader>ca  <Plug>(coc-codeaction-selected)


" Remap keys for applying codeAction to the current buffer.
nmap <leader>cac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>ch   <Plug>(coc-fix-current)

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

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <leader>la  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <leader>le  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <leader>lc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <leader>lo  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <leader>ls  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <leader>lj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <leader>lk  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <leader>lp  :<C-u>CocListResume<CR>

" Set up prettier command
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" Set up ClangFormat command
command! -nargs=0 Prettier :

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)




set statusline=
set statusline+=%<\                       " cut at start
set statusline+=%2*[%n%H%M%R%W]%*\        " flags and buf no
set statusline+=%-40f\                    " path
set statusline+=%=%1*%y%*%*\              " file type
set statusline+=%10((%l,%c)%)\            " line and column
set statusline+=%P                        " percentage of file


let g:coc_global_extensions = [
	\ 'coc-clangd',
	\ 'coc-css',
	\ 'coc-emmet',
	\ 'coc-emoji',
	\ 'coc-eslint',
	\ 'coc-git',
	\ 'coc-go',
	\ 'coc-highlight',
	\ 'coc-html',
	\ 'coc-json',
	\ 'coc-jedi',
	\ 'coc-marketplace',
	\ 'coc-prettier',
	\ 'coc-rls',
	\ 'coc-snippets',
	\ 'coc-tslint-plugin',
	\ 'coc-tsserver',
	\ 'coc-vimtex'
  	\ ]


""" ALE
let g:ale_sign_error = 'X'
let g:ale_sign_warning = '!'

nmap <silent> [c <Plug>(ale_previous_wrap)
nmap <silent> ]c <Plug>(ale_next_wrap)



""" ULTISNIPS
let g:UltiSnipsForwardTrigger="<c-n>"


"""" LANGUAGE SERVER
"" Vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
"" TODO Reconsider
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'
nnoremap <leader>p :silent execute "grep! -R " . shellescape(expand("<cWORD>")) . " ."<cr>:copen<cr>

