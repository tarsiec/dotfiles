-- BACKGROUND
vim.g.term = "alacritty"
vim.g.base16colorspace = "256"
set.termguicolors = true
set.background = "dark"

-- THEME
-- (solarized)
vim.g.solarized_visibility = "low"
vim.g.solarized_statusline = "flat"
vim.g.solarized_diffmode = "low"
vim.cmd([[ colorscheme tokyonight ]])

-- LINE
set.cursorline = true -- show current line

-- SEARCH APPEARANCE
set.hlsearch = false -- dont highlight everything

-- HIDDEN CHARS
-- vim.cmd([[ hi SpecialKey guifg=black ctermfg=darkgrey ]])
-- vim.cmd([[ set listchars=tab:>\ ]])
set.list = false

-- NUMBERS
set.number = true
set.relativenumber = true

-- WHICH KEY
local which_key = require("which-key")
which_key.setup{}

-- LSPSAGA
-- saga = require("lspsaga")
-- saga.init_lsp_saga{}

-- NVIM TREE
nvim_tree = require("nvim-tree")
config = nvim_tree.setup{
	auto_close = false,
	auto_reload_on_write = true,
	disable_netrw = false,
	hide_root_folder = false,
	hijack_cursor = false,
	hijack_netrw = true,
	hijack_unnamed_buffer_when_opening = false,
	ignore_buffer_on_setup = false,
	open_on_setup = false,
	open_on_tab = false,
	sort_by = "name",
	update_cwd = false,
	view = {
		width = 30,
		height = 30,
		side = "left",
		preserve_window_proportions = false,
		number = false,
		relativenumber = false,
		signcolumn = "yes",
		mappings = {
			custom_only = false,
			list = {
				-- user mappings go here
			},
		},
	},
	hijack_directories = {
		enable = true,
		auto_open = true,
	},
	update_focused_file = {
		enable = false,
		update_cwd = false,
		ignore_list = {},
	},
	ignore_ft_on_setup = {},
	system_open = {
		cmd = nil,
		args = {},
	},
	diagnostics = {
		enable = false,
		show_on_dirs = false,
		icons = {
			hint = "",
			info = "",
			warning = "",
			error = "",
		},
	},
	filters = {
		dotfiles = false,
		custom = {},
		exclude = {},
	},
	git = {
		enable = true,
		ignore = true,
		timeout = 400,
	},
	actions = {
		change_dir = {
			enable = true,
			global = false,
		},
		open_file = {
			quit_on_open = false,
			resize_window = false,
			window_picker = {
				enable = true,
				chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
				exclude = {
					filetype = { "notify", "packer", "qf", "diff", "fugitive", "fugitiveblame" },
					buftype = { "nofile", "terminal", "help" },
				},
			},
		},
	},
	trash = {
		cmd = "trash",
		require_confirm = true,
	},
	log = {
		enable = false,
		truncate = false,
		types = {
			all = false,
			config = false,
			git = false,
		},
	},
}

-- STATUSLINE
local lualine = require("lualine").setup{
	options = {
		icons_enabled = true,
		theme = 'auto',
		component_separators = { left = '', right = ''},
		section_separators = { left = '', right = ''},
		disabled_filetypes = {},
		always_divide_middle = true,
		globalstatus = false,
	},
	sections = {
		lualine_a = {'mode'},
		lualine_b = {'branch', 'diff', 'diagnostics'},
		lualine_c = {'filename'},
		lualine_x = {'encoding', 'fileformat', 'filetype'},
		lualine_y = {'progress'},
		lualine_z = {'location'}
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {'filename'},
		lualine_x = {'location'},
		lualine_y = {},
		lualine_z = {}
	},
	tabline = {},
	extensions = { "nvim-tree", "toggleterm", "fugitive", "fzf" }
}

-- SYMBOLS OUTLINE
-- vim.g.symbols_outline = {}

-- SIMPLYFOLD
vim.g.SimplyFold_docstring_preview = 1
