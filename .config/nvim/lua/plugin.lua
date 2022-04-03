vim.cmd [[ packadd packer.nvim ]]

return require("packer").startup(function()
-- 	-- PACKER ITSELF
	use "wbthomason/packer.nvim"

-- 	-- LSP
	use "neovim/nvim-lspconfig"
	use {
		"ojroques/nvim-lspfuzzy",
		requires = {
			"junegunn/fzf",
			"junegunn/fzf.vim"
		}
	}
	use "onsails/lspkind-nvim" -- icons
	use "ray-x/lsp_signature.nvim"
	use "mfussenegger/nvim-dap"

	-- LANGS
	use "fatih/vim-go"
	use "lervag/vimtex" -- tex
	use "puremourning/vimspector"
	use "nvim-telescope/telescope-dap.nvim"
	use {
		"rust-lang/rust.vim",
		requires = {
			"mattn/webapi-vim"
		}
	}
	use "mattn/emmet-vim"
	use "dag/vim-fish"

	-- GIT
	use {
		"lewis6991/gitsigns.nvim",
		requires = {
			"nvim-lua/plenary.nvim"
		}
	}
	use "jreybert/vimagit"
	use "tpope/vim-fugitive"

	-- SYNTAX
	use "euclidianAce/BetterLua.vim"
	use "junegunn/goyo.vim" -- writing mode
	use {
		"nvim-treesitter/nvim-treesitter",
		requires = {
			"kyazdani42/nvim-web-devicons",
			opt = true
		}
	}

	-- UI
	use {
		'kyazdani42/nvim-tree.lua',
		requires = {
			'kyazdani42/nvim-web-devicons', -- optional, for file icons
		}
	}
	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			"nvim-lua/plenary.nvim"
		}
	}
	use "akinsho/toggleterm.nvim"
	use "folke/which-key.nvim"
	use "DanilaMihailov/beacon.nvim"
	use "nvim-lualine/lualine.nvim"
	use "simrat39/symbols-outline.nvim"

	-- COMP
	use "hrsh7th/nvim-cmp" -- configurations
	use 'hrsh7th/cmp-nvim-lsp' -- LSP source for nvim-cmp
	use "SirVer/Ultisnips"

	-- THEME
	use "arcticicestudio/nord-vim"
	use "altercation/vim-colors-solarized"
	use "lifepillar/vim-solarized8"
	use "folke/tokyonight.nvim"
	use "jnurmine/zenburn"
	use "morhetz/gruvbox"
	use "chriskempson/base16-vim"

	-- HELPFUL
	use "svermeulen/vimpeccable"
	use "LionC/nest.nvim"
	use "wellle/targets.vim"
	use "tpope/vim-surround"
	use "tpope/vim-repeat"
	use "cohama/lexima.vim" -- close brackets
	use "tpope/vim-commentary"
	use "rhysd/clever-f.vim"
	use "tmhedberg/matchit"
	use "alvan/vim-closetag"
	use "turbio/bracey.vim"
	use "tmhedberg/SimpylFold"
end)
