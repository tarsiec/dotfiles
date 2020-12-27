" NEOVIM CONFIGURATION: THIS TIME WITH STRUCTURE EDITION

call plug#begin("~/.config/nvim/plugins")

"" SYNTAX HIGHLIGHTING & COMPLETION
" Syntax
Plug 'dense-analysis/ale'
" Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Snippets
Plug 'sirver/ultisnips'
Plug 'honza/vim-snippets'

"" LANGS
" LaTeX
Plug 'lervag/vimtex'

" MOTION
Plug 'jiangmiao/auto-pairs'

"" FILESYSTEM
" NERDTree
Plug 'preservim/nerdtree'
" Ctrl-P
Plug 'ctrlpvim/ctrlp.vim'

"" GIT
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'

"" AESTHETICAL
" Base16 for vim
Plug 'chriskempson/base16-vim'
Plug 'arcticicestudio/nord-vim'
" Devicons
Plug 'ryanoasis/vim-devicons'
" Lightline
Plug 'itchyny/lightline.vim'
Plug 'mike-hearn/base16-vim-lightline'
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
filetype plugin indent on
set termguicolors
set background=dark
set t_Co=256
colorscheme base16-tomorrow-night
set nu rnu

set laststatus=2
set noshowmode

let g:lightline = {
            \ 'colorscheme': 'base16_tomorrow_night'
            \ }


"TODO CHANGE NAME OF THIS SECTION
"""" MOTION & SHORTCUTS 

""" MAPPINGS
"" TODO Reconsider these
"" Go back to normal mode easily
inoremap jk <esc>

"" Disable some shortcuts
"iunnoremap <c-j>
"iunnoremap <c-k>
"iunnoremap <c-o>
"iunnoremap <c-p>


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


"""" SYNTAX HILIGHTING & COMPLETION
"" ALE
"nnoremap <leader>gd :ALEGoToDefinition<cr>
"nnoremap <leader>gr :ALEFindReferences<cr>
"nnoremap <leader>gh :ALEHover<cr>
"nnoremap <leader>gs :ALESymbolSearch<cr>

"" COC.NVIM
set hidden
set nobackup
set nowritebackup
" more space for messages
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=auto

" extensions
let g:coc_global_extensions = [
            \ 'coc-snippets',
            \ 'coc-lists',
            \ 'coc-ccls',
            \ 'coc-rls',
            \ 'coc-go',
            \ 'coc-git',
            \ 'coc-vimtex',
            \ 'coc-tabnine',
            \ 'coc-json',
            \ 'coc-prettier',
            \ 'coc-html',
            \ 'coc-css',
            \ 'coc-emmet',
            \ 'coc-highlight'
            \ ]

" <c-space> for completion
inoremap <silent><expr> <c-space> coc#refresh()

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction


inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"


" [g and ]g to navigate diagnostic
nnoremap <silent> [g <Plug>(coc-diagnostic-prev)
nnoremap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nnoremap <silent> gd <Plug>(coc-definition)
nnoremap <silent> gy <Plug>(coc-type-definition)
nnoremap <silent> gi <Plug>(coc-implementation)
nnoremap <silent> gr <Plug>(coc-references)

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

" Highlight symbol under cursor on CursorHold
"autocmd CursorHold * silent call CocActionAsync('highlight')


" Symbol renaming.
nnoremap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xnoremap <leader>f  <Plug>(coc-format-selected)
nnoremap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xnoremap <leader>a  <Plug>(coc-codeaction-selected)
nnoremap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nnoremap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nnoremap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xnoremap if <Plug>(coc-funcobj-i)
onoremap if <Plug>(coc-funcobj-i)
xnoremap af <Plug>(coc-funcobj-a)
onoremap af <Plug>(coc-funcobj-a)
xnoremap ic <Plug>(coc-classobj-i)
onoremap ic <Plug>(coc-classobj-i)
xnoremap ac <Plug>(coc-classobj-a)
onoremap ac <Plug>(coc-classobj-a)

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
nnoremap <silent> <C-s> <Plug>(coc-range-select)
xnoremap <silent> <C-s> <Plug>(coc-range-select)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nnoremap <silent> <C-s> <Plug>(coc-range-select)
xnoremap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>sd  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>ce  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>sc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>so  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>ss  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>aj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>ak  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>ap  :<C-u>CocListResume<CR>

" coc highlight
autocmd CursorHold * silent call CocActionAsync('highlight')


"" UltiSnips
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'
let g:UltisnipsSnippetDirectories=["UltiSnips", "plugins/vim-snippets/UltiSnips"]

"""" LANGUAGE SERVER
"" Vimtex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
"" TODO Reconsider
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'


