local o	= vim.o
local g = vim.g
local w	= vim.wo
local b	= vim.bo
local api = vim.api
local cmd = vim.cmd

-- key leaders
g.mapleader 	= ' '
g.localleader	= '\\'

-- swap
o.swapfile		= true
o.dir			= '/tmp/'

-- search
o.smartcase		= true
o.ignorecase	= true
o.hlsearch		= false
o.incsearch		= true

-- statusline size
o.laststatus	= 2

-- handle tabs
o.tabstop		= 4
o.softtabstop	= 4
o.shiftwidth	= 4
o.expandtab		= false
o.smartindent	= true

-- render whitespace
o.list			= false

-- use my dir's vimrc
o.exrc			= true

-- better line numbers
o.nu			= true
o.rnu			= true

-- substitute preview
o.inccommand	= 'nosplit'

-- hidden buffers still on
o.hidden		= true

-- don't annoy me with swap files
--o.backup		= false
o.undodir		= '~/.config/nvim/undodir'

-- more centered cursor while scrolling
o.scrolloff		= 4

-- extra column for plugins
o.signcolumn	= 'auto'

-- don't make lines too long (for now off)
o.colorcolumn	= '0'
cmd('highlight ColorColumn ctermbg = darkgray')

-- allow scrolling when scrolling should be allowed
o.mouse			= 'nv'

-- the single best mapping in this config
api.nvim_set_keymap('i', 'jk', '<esc>', {noremap=true, silent=true})

-- split right
o.splitright	= true
o.splitbelow	= true

-- map gf to :e
api.nvim_set_keymap('n', 'gf', ':e <cfile><cr>', {noremap=true, silent=true})
