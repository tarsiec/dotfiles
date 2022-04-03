-- MAP NEST
-- local vimp = require("vimp")
local nest = require("nest")
nest.applyKeymaps {
	{ "<leader>", { -- common prefix
		{ "c", { -- code
			{ "a", "<cmd>lua vim.lsp.buf.code_action()<cr>" }, -- code actions
			{ "d", "<cmd>lua vim.lsp.buf.definition()<cr>" }, -- goto definition
			{ "f", "<cmd>lua vim.lsp.buf.formatting()<cr>" }, -- format
			{ "n", "<cmd>lua vim.lsp.buf.rename()<cr>" }, -- rename
			{ "r", "<cmd>lua vim.lsp.buf.references()<cr>" }, -- references
			{ "s", "<cmd>lua vim.lsp.buf.document_symbol()<cr>" }, -- doc symbol
		}},
		{ "d", { -- debug
			{ "b", "<cmd>lua dap.toggle_breakpoint()<cr>" }, -- toggle breakpoint
			{ "c", "<cmd>lua dap.continue()<cr>" }, -- continue
			{ "v", { -- variables
				{ "h", "<cmd>lua require('dap.ui.variables').hover()<cr>" }, -- hover
				{ "s", "<cmd>lua require('dap.ui.variables').scopes()<cr>" }, -- scopes
				{ "h", "<cmd>lua require('dap.ui.variables').hover_visual()<cr>" }, -- hover (visual)
			}},
			{ "s", { -- step
				{ "i", "<cmd>lua dap.step_into()<cr>" }, -- into
				{ "o", "<cmd>lua dap.out()<cr>" }, -- out
				{ "v", "<cmd>lua dap.step_over()<cr>" }, -- over
			}},
			{ "r", {
				{ "o", "<cmd>lua dap.repl.open()<cr>" }, -- open repl
				{ "r", "<cmd>lua dap.repl.run_last()<cr>" }, -- restore repl
			}},
			{ "t", {
				{ "b", "<cmd>lua telescope.extensions.dap.list_breakpoints{}<cr>" },
				{ "c", "<cmd>lua telescope.extensions.dap.commands{}<cr>" },
				{ "f", "<cmd>lua telescope.extensions.dap.frames{}<cr>" },
				{ "s", "<cmd>lua telescope.extensions.dap.configurations{}<cr>" },
				{ "v", "<cmd>lua telescope.extensions.dap.variables{}<cr>" },
			}},
		}},
		{ "e", { -- edit
			{ "s", "<cmd>sp $MYVIMRC<cr>" }, -- vimrc in split
			{ "v", "<cmd>e $MYVIMRC<cr>" }, -- vimrc
		}},
		{ "f", { -- find
			{ "f", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>" },
			{ "F", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>" },
			{ "t", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>" },
			{ "T", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>" },
		}},
		{ "g", { -- git
			{ "a", "<cmd>Git add %<cr>" },
			{ "c", "<cmd>Git commit<cr>" },
			{ "d", "<cmd>Git diff<cr>" },
			{ "l", "<cmd>Git log<cr>" },
			{ "m", "<cmd>Git merge<cr>" },
			{ "t", {
				{ "d", "<cmd>Git difftool<cr>" },
				{ "m", "<cmd>Git mergetool<cr>" },
			}},
			{ "s", "<cmd>Gdiffsplit<cr>" },
			{ "w", "<cmd>Gwrite<cr>" },
			{ ">", "<cmd>Git push<cr>" },
			{ "<", "<cmd>Git pull<cr>" },
			{ "'", "<cmd>Magit<cr>" }, -- vimagit
			{ ".", "<cmd>Gread<cr>" },
		}},
		{ "o", { -- open/close
			{ "b", "<cmd>Git blame<cr>" }, -- Git blame
			{ "n", "<cmd>NvimTreeToggle<cr>" }, -- Nvim Tree
			{ "N", "<cmd>NvimTreeFindFile<cr>" }, -- Nvim Tree find file
			{ "t", "<cmd>ToggleTerm<cr>" }, -- ToggleTerm
			{ "s", "<cmd>SymbolsOutline<cr>" }, -- Toggle symbols outline
		}},
		{ "p", { -- command palette
			{ "b", "<cmd>Telescope buffers<cr>" },
			{ "f", "<cmd>Telescope find_files<cr>" }, -- find files
			{ "g", "<cmd>Telescope live_grep<cr>" },
			{ "m", "<cmd>Telescope man_pages<cr>" },
			{ " ", {
				{ "d", "<cmd>Telescope lsp_definitions<cr>" },
				{ "i", "<cmd>Telescope lsp_definitions<cr>" },
				{ "t", "<cmd>Telescope lsp_definitions<cr>" },
				{ "r", "<cmd>Telescope lsp_definitions<cr>" },
				{ "s", "<cmd>Telescope lsp_definitions<cr>" },
				{ "w", "<cmd>Telescope lsp_definitions<cr>" },
			}},
			{ "/", "<cmd>Telescope live_grep<cr>" }, -- Telescope grep
		}},
		{ "s", { -- source
			{ "f", "<cmd>luafile %<cr>" },
			{ "i", "<cmd>PackerInstall<cr>" },
			{ "v", "<cmd>luafile $MYVIMRC<cr>" },
		}},
		{ "t", { -- toggle
			{ "c", "<cmd>Telescope colorscheme<cr>" }, -- toggle writing mode
			{ "b", function() -- toggle background
				if vim.opt.background:get() == "dark"
				then set.background = "light"
				else set.background = "dark" end
			end}, 
			{ "n", function() vim.cmd([[ set number! relativenumber! ]]) end }, -- line numbers
			{ "w", "<cmd>Goyo<cr>" }, -- toggle writing mode
		}},
		{ "w", { -- window
			{ "h",  "<c-w>h<cr>"}, -- move
			{ "j",  "<c-w>j<cr>"},
			{ "k",  "<c-w>k<cr>"},
			{ "l",  "<c-w>l<cr>"},
			{ "H",  "<cmd>vertical resize +5<cr>"}, -- resize
			{ "J",  "<cmd>resize +5<cr>"},
			{ "K",  "<cmd>resize -5<cr>"},
			{ "L",  "<cmd>vertical resize -5<cr>"},
		}}
	}},
	{ "g", {
		{ "[", "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>" }, -- goto prev diagnostic
		{ "]", "<cmd>lua vim.lsp.diagnostic.goto_next()<cr>" }, -- goto next diagnostic
		{ "d", "<cmd>lua vim.lsp.buf.definition()<cr>" }, -- goto definition
		{ "n", "<cmd>lua vim.lsp.buf.rename()<cr>" }, -- rename
		{ "r", "<cmd>lua vim.lsp.buf.references()<cr>" }, -- references
	}},
	{ "K", "<cmd>lua vim.lsp.buf.hover()<cr>" },
	{ "<c-h>", "<c-w>h" }, -- window left
	{ "<c-j>", "<c-w>j" }, -- window up
	{ "<c-k>", "<c-w>k" }, -- window down
	{ "<c-l>", "<c-w>l" }, -- window right
	{ "<c-s-d>", "<cmd>wq<cr>" }, -- quit, saving
	{ mode = "i", {
		{ "jk", "<esc>" },
		{ "<a-h>", "<c-w>h" },
		{ "<a-j>", "<c-w>j" },
		{ "<a-k>", "<c-w>k" },
		{ "<a-l>", "<c-w>l" },
	}},
	{ mode = "t", {
		{ "<esc>", [[ <c-\><c-n> ]] },
		{ "jk",    [[ <c-\><c-n> ]] },
		{ "<c-h>", [[ <c-\><c-n><c-w>h ]] },
		{ "<c-j>", [[ <c-\><c-n><c-w>j ]] },
		{ "<c-k>", [[ <c-\><c-n><c-w>k ]] },
		{ "<c-l>", [[ <c-\><c-n><c-w>l ]] },
	}},
	{ mode = "o", {
		{ "f", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, inclusive_jump = true })<cr>" },
		{ "F", "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, inclusive_jump = true })<cr>" },
	}},
}

vim.cmd([[ inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>" ]])
vim.cmd([[ inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<TAB>" ]])
