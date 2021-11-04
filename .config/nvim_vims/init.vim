""" NEOVIM CONFIG
call plug#begin('~/.config/nvim/plugins')
	""" BACKEND
	"" Lua
	Plug 'nvim-lua/popup.nvim'
	Plug 'nvim-lua/plenary.nvim'

	""" LANGUAGES
	"" GO
	" Plug 'fatih/vim-go'
	"" Haskell
	" Plug 'neovimhaskell/haskell-vim'
	"" LaTeX
	Plug 'lervag/vimtex'
	Plug   'KeitaNakamura/tex-conceal.vim', {'for': 'tex'}

	""" COMPLETION
	"" LSP
	Plug 'neovim/nvim-lspconfig'
	Plug 'hrsh7th/cmp-nvim-lsp'
	Plug 'hrsh7th/cmp-buffer'
	Plug 'hrsh7th/nvim-cmp'
	"" Snippets
	Plug 'SirVer/ultisnips'
	Plug 'honza/vim-snippets'

	" Plug 'neoclide/coc.nvim', {'branch': 'release'}
	" Plug 'nvim-lua/completion-nvim'
	"" CC
	" Plug 'xavierd/clang_complete'
	"" Which key
	Plug 'liuchengxu/vim-which-key'
	"" Parens
	Plug 'windwp/nvim-autopairs'

	""" UI
	"" Syntax highlighting
	Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
	"" File mgmt
	Plug 'scrooloose/nerdtree'
	"" FzF
	Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
	Plug 'nvim-treesitter/nvim-treesitter-refactor'
	"" Frameworks
	Plug 'nvim-lua/popup.nvim'
	Plug 'nvim-lua/plenary.nvim'
	Plug 'nvim-telescope/telescope.nvim'
	Plug 'nvim-telescope/telescope-media-files.nvim'

	""" GIT
	Plug 'Xuyuanp/nerdtree-git-plugin'
	Plug 'airblade/vim-gitgutter'

	""" MOVEMENT
	Plug 'tpope/vim-surround'
	Plug 'tpope/vim-commentary'
	Plug 'tpope/vim-fugitive'

	""" THEMEING
	Plug 'chriskempson/base16-vim'
	Plug 'gruvbox-community/gruvbox'
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'ryanoasis/vim-devicons'
call plug#end()

""" PREFACE
let mapleader=" "
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>


""" VIM BASIC
source ~/.config/nvim/resources/basic.vim
""" THEMEING
source ~/.config/nvim/resources/theme.vim
""" GLOBAL REMAPS
source ~/.config/nvim/resources/globalmap.vim
""" FILE MOVEMENT
source ~/.config/nvim/resources/filemov.vim
""" COMPLETION
source ~/.config/nvim/resources/completion.vim
""" LSP
source ~/.config/nvim/resources/lsp.vim

""" LANGS
"" LaTeX
" autocmd BufNewFile,BufRead *.tex source ~/.config/nvim/resources/langs/tex.vim
augroup ft_tex
	let g:tex_flavor='latex'
	let g:vimtex_view_method='zathura'
	let g:vimtex_quickfix_mode=0
	let g:vimtex_fold_enabled=0
	let g:tex_conceal='abdmg'
	let g:UltiSnipsExpandTrigger = '<tab>'
    let g:UltiSnipsJumpForwardTrigger = '<tab>'
    let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
	autocmd BufNewFile,BufRead *.tex set conceallevel=2
	autocmd BufNewFile,BufRead *.tex nnoremap <leader>cc :VimtexCompile<cr>
	autocmd BufNewFile,BufRead *.tex nnoremap <leader>cv :VimtexView<cr>
	autocmd BufNewFile,BufRead *.tex nnoremap <leader>ct :VimtexTocToggle<cr>
	autocmd BufNewFile,BufRead *.tex nnoremap <leader>cw :VimtexCountWords<cr>
	autocmd BufNewFile,BufRead *.tex set spell
augroup end

"" Python
" autocmd Filetype python source ~/.config/nvim/resources/langs/python.vim
"" Go
" autocmd Filetype go source ~/.config/nvim/resources/langs/golang.vim
"" CC
" autocmd Filetype c source ~/.config/nvim/resources/langs/cc.viTocTogglTocTogglTocToggleeem
