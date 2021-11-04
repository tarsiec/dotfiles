local function setmap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
local function setopt(...) vim.api.nvim_buf_set_option(bufnr, ...) end
local opts = { noremap = true, silent = true }

-- Find <string> in files
setmap('n', '<space>pf', '<cmd>Telescope find_files<cr>', opts)

-- Find <string> in files
setmap('n', '<space>p/', '<cmd>:Telescope live_grep<cr>', opts)

-- Find symbols in code
setmap('n', '<space>ps', '<cmd>Telescope treesitter<cr>', opts)

-- Find man pages
setmap('n', '<space>pm', '<cmd>Telescope man_pages<cr>', opts)

-- Find vim options
setmap('n', '<space>pv', '<cmd>Telescope vim_options<cr>', opts)
