-- utility  --
-- vars
local opts = { noremap = true, silent = true }
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

-- funcs
local function setmap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
local function setopt(...) vim.api.nvim_buf_set_option(bufnr, ...) end

-- lspconfig --
local nvim_lsp = require('lspconfig')
local protocol = require('vim.lsp.protocol')
-- lspsaga --
local saga = require('lspsaga')


local on_attach = function(client, bufnr)
	-- goto
	setmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', opts)		-- show definition
	setmap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', opts)		-- show declaration
	setmap('n', 'gh', '<cmd>Lspsaga lsp_finder<cr>', opts)					-- show ref. [lspsaga]
	setmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<cr>', opts)	-- show implementation
	setmap('n', 'gt', '<cmd>lua vim.lsp.buf.type_definition()<cr>', opts)	-- show implementation
	setmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<cr>', opts)		-- show references

	-- rename
	setmap('n', '<f2>', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)			-- rename symbol
	setmap('n', '<leader>cr', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)	-- rename symbol

	-- diagnostics
	setmap('i', '<c-j>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>', opts)		-- next error
	setmap('i', '<c-k>', '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>', opts)		-- next error
	setmap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>', opts)		-- next error
	setmap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>', opts)		-- next error

	-- types
	setmap('i', '<c-k>', '<cmd>Lspsaga signature_help<cr>', opts)
	setmap('n', '<leader>cs', '<cmd>Lspsaga signature_help<cr>', opts)
	setmap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<cr>', opts)
	--setmap('n', 'K', '<cmd>Lspsaga hover_doc<cr>', opts)

	-- code actions
	setmap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<cr>', opts)

	-- formatting
	setmap('n', '<leader>cf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- LANGS
local servers = { 'pyright', 'rust_analyzer', 'tsserver', 'gopls' }
for _, lsp in ipairs(servers) do
	nvim_lsp[lsp].setup {
		on_attach = on_attach,
		capabilities = capabilities,
		flags = {
			debounce_text_changes = 150
		}
	}
end

saga.init_lsp_saga {
	border_style = "round",
	error_sign = '',
	warn_sign = '',
	hint_sign = '',
	infor_sign = '',
	dianostic_header_icon = '   ',
	code_action_icon = ' ',
	finder_definition_icon = '  ',
	finder_reference_icon = '  ',
	rename_prompt_prefix = '>',
	max_preview_lines = 10, -- preview lines of lsp_finder and definition preview

	finder_action_keys = {
	  open = 'o', vsplit = 's',split = 'i',quit = 'q',scroll_down = '<C-f>', scroll_up = '<C-b>' -- quit can be a table
	},
	code_action_keys = {
	  quit = 'q',exec = '<CR>'
	},
	rename_action_keys = {
	  quit = '<C-c>',exec = '<CR>'  -- quit can be a table
	},
	definition_preview_icon = '  '
}

