-- theme
vim.g.colors_name		= 'nord'
vim.g.base16colorspace	= 256
vim.o.background		= 'dark'

-- allow 256 colorspace
vim.o.termguicolors		= true

-- Change color theme
vim.cmd('nnoremap <leader>tt :Colors<cr>')

-- Highlight the row
vim.o.cursorcolumn = false
vim.o.cursorline = true

-- Transparency
vim.cmd('highlight Normal ctermbg=NONE guibg=NONE')
vim.cmd('highlight LineNr ctermbg=NONE guibg=NONE')

-- Airline config
-- air-line
--g:airline_powerline_fonts = 1

-- Galaxyline.nvim
