vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
	-- General
	use 'wbthomason/packer.nvim'
	use 'cohama/lexima.vim'
	use 'kyazdani42/nvim-web-devicons'
	-- use 'yamatsum/nvim-nonicons'
	-- use 'onsails/lspkind-nvim'

	-- Natural extensions
	use 'tpope/vim-fugitive'
	use 'tpope/vim-rhubarb'
	use 'tpope/vim-surround'
	use 'tpope/vim-commentary'

	-- Syntax highlighting
	use 'euclidianAce/BetterLua.vim'

	-- LSP
	use 'neovim/nvim-lspconfig'
	use 'glepnir/lspsaga.nvim'
	-- use 'hrsh7th/nvim-cmp'
	-- use {'neoclide/coc.nvim', branch='release'}
	
	-- Completion
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/cmp-cmdline'
	use 'hrsh7th/nvim-cmp'

	-- Linting
	use 'nvim-treesitter/nvim-treesitter'

	-- Finding stuff
	use 'nvim-lua/plenary.nvim'
	use 'nvim-telescope/telescope.nvim'


	-- Themeing
	use 'chriskempson/base16-vim'
	use 'morhetz/gruvbox'
	use 'sainnhe/gruvbox-material'
	use 'arcticicestudio/nord-vim'
	use {
		'glepnir/galaxyline.nvim',
		branch = main,
		config = function() require('my_statusline') end,
		requires = {'kyazdani42/nvim-web-devicons', opt = true}
	}
end)

