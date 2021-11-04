"" Easy sourcing
nnoremap <leader>sv :source ~/.config/nvim/init.vim<cr>
nnoremap <leader>si :source ~/.config/nvim/init.vim<cr>>:PlugInstall<cr>
nnoremap <leader>ev :sp ~/.config/nvim/init.vim<cr>

"" Handle tabs
set tabstop=4 softtabstop=4
set shiftwidth=4
set noexpandtab
set smartindent

"" Render whitespace
set nolist

"" Use your dir's vimrc
set exrc

"" Better line numbers
set nu
set rnu

"" Smarter search behaviour
set incsearch
set nohlsearch
set smartcase
set ignorecase

"" Substitute preview
set inccommand=nosplit

"" Hidden buffers still on
set hidden

"" Don't annoy me with swap files
set noswapfile
set nobackup
set undodir=~/.config/nvim/undodir

"" More centered cursor while scrolling
set scrolloff=4

"" Extra column for plugins
set signcolumn=auto

"" Don't make lines too long (for now off)
" set colorcolumn=80
" highlight ColorColumn ctermbg=darkgray

"" Allow scrolling when scrolling should be allowed
set mouse=nv

"" The single best mapping in this config
inoremap jk <esc>

"" Split right
set splitright splitbelow

"" Source vimscripts  in dir
set exrc

"" Folding
set foldmethod=expr
