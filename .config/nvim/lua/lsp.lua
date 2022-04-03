-- TREESITTER
local ts = require("nvim-treesitter.configs")
ts.setup {
	ensure_installed = "maintained", -- install parsers
	sync_install = false, -- install synchronously
	highlight = {
		enable = true,
		disable = { "html", "markdown", "fish" },
	},
}

-- LSPCONFIG
lspconf = require("lspconfig")
servers = { "pyright", "rust_analyzer", "tsserver", "gopls", "texlab", "html", "cssls" }
			-- "html", "cssls" }

vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(
            vim.lsp.handlers.signature_help, {
                border = 'rounded',
                close_events = {"CursorMoved", "BufHidden", "InsertCharPre" }})

vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
            vim.lsp.handlers.hover, { border = 'rounded' })

local capabilities = vim.lsp.protocol.make_client_capabilities()

for _, lsp in pairs(servers) do
	lspconf[lsp].setup{
		capabilities = capabilities,
		on_attach = on_attach,
		handlers = handlers,
		flags = {},
	}
end

require'lspconfig'.tsserver.setup{
	filetypes = { "typescript", "typescriptreact", "typescript.tsx" },
	root_dir = function() return vim.loop.cwd() end      -- run lsp for javascript in any directory
}

local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end


-- LSPFUZZY
-- local lspfuzzy = require("lspfuzzy")
-- lspfuzzy.setup{
-- 	methods = 'all',         -- either 'all' or a list of LSP methods (see below)
-- 	jump_one = true,         -- jump immediately if there is only one location
-- 	save_last = false,       -- save last location results for the :LspFuzzyLast command
-- 	callback = nil,          -- callback called after jumping to a location
-- 	fzf_preview = {          -- arguments to the FZF '--preview-window' option
-- 		'right:+{2}-/2'          -- preview on the right and centered on entry
-- 	},
-- 	fzf_action = {               -- FZF actions
-- 		['ctrl-t'] = 'tab split',  -- go to location in a new tab
-- 		['ctrl-v'] = 'vsplit',     -- go to location in a vertical split
-- 		['ctrl-x'] = 'split',      -- go to location in a horizontal split
-- 	},
-- 	fzf_modifier = ':~:.',   -- format FZF entries, see |filename-modifiers|
-- 	fzf_trim = true,         -- trim FZF entries
-- }

-- LSP SIGNATURE
local lspsig = require("lsp_signature")
lspsig.setup{
	hint_enable = false,
	hint_prefix = "> ",
	handler_opts = {
		border = "rounded"
	}
}

-- DEBUGGING
dap = require("dap")
-- dap_ui_variables = require("dap.ui.variables")
-- dap_ui_widgets = require("dap.ui.widgets")

