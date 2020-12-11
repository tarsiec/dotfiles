" NEOVIM CONFIGURATION: THIS TIME WITH STRUCTURE EDITION

call plug#begin("~/.config/nvim/plugins")

" Base16 for vim
Plug 'chriskempson/base16-vim'
call plug#end()

" for global plugins
let mapleader = " "
" for buffer specific mappings
let maplocalleader = "\\"

"""" APPEARANCE
set termguicolors
colorscheme base16-tomorrow-night
set nu rnu


"TODO CHANGE NAME OF THIS SECTION
"""" MOTION & SHORTCUTS 

""" =[BUFFER AGNOSTIC]=
""" REMAPS
"" TODO Reconsider these
"" Go back to normal mode easily
inoremap jk <esc>

"" TODO Remember this is maybe temporary
"" Force myself to not use esc
inoremap <esc> <nop>

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

"" TODO Remove these
"" Go to [x] of the line
" - beginning
nnoremap H 0
" - end
nnoremap L $

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


"""" FILES, PROJECTS & VERSION MANAGEMENT


"""" SYNTAX HILIGHTING


"""" LANGUAGE SERVER

