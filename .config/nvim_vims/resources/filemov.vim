"" Find <string> in files
nnoremap <leader>pf :Telescope find_files<cr>

"" Find <string> in files
nnoremap <leader>p/ :Telescope live_grep<cr>

"" Find symbols in code
nnoremap <leader>ps :Telescope treesitter<cr>

"" Find man pages
nnoremap <leader>pm :Telescope man_pages<cr>

"" Find vim options
nnoremap <leader>pv :Telescope vim_options<cr>

" configure treesitter
lua << EOF
require'nvim-treesitter.configs'.setup {
	highlight = {
		enable = true,
		disable = {},
		updatetime = 25,
		persist_queries = false
	},
}
EOF
