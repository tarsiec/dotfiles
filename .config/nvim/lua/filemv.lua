-- TELESCOPE
telescope = require("telescope")
telescope.setup {
	defaults = {},
	pickers = {},
	extensions = {}
}

-- TOGGLETERM
require("toggleterm").setup{
	float_opts = {
		border = "double"
	}
}
