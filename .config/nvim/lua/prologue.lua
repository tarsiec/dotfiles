set = vim.opt
-- LEADER
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- CASES
set.ignorecase = true
set.smartcase = true
set.incsearch = true

-- BUFFERS
set.hidden = true

-- HISYORY
set.history = 5000

-- TABSTOP
set.tabstop = 4
set.shiftwidth = vim.o.tabstop
set.smarttab = false
set.expandtab = false
set.autoindent = true

-- COLUMN
set.colorcolumn = "0"

-- SCROLL
set.scrolloff = 4
set.sidescrolloff = 8

-- WRAP
set.wrap = false

-- SYNTAX
vim.cmd([[ syntax enable ]])
vim.cmd([[ filetype plugin indent on ]])

-- SPLIT RIGHT
set.splitbelow = true
set.splitright = true

-- FOLDING
set.foldmethod = "indent"
set.foldlevel = 99
