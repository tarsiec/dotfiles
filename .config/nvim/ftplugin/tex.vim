lua << EOF
vim.opt.wrap = true

vim.g.tex_flavor="latex"
vim.g.vimtex_view_enabled=1
vim.g.vimtex_view_automatic=1
vim.g.vimtex_view_method="zathura"
vim.g.vimtex_view_general_viewer="zathura"
vim.g.vimtex_quickfix_mode=0
vim.g.vimtex_fold_enabled=1
vim.g.tex_conceal="abdmg"
vim.g.UltiSnipsExpandTrigger = "<tab>"
vim.g.UltiSnipsJumpForwardTrigger = "<c-j>"
vim.g.UltiSnipsJumpBackwardTrigger = "<c-k>"
vim.g.UltiSnipsEditSplit="vertical"
EOF
set conceallevel=2

nnoremap <leader>cc :VimtexCompile<cr>
nnoremap <leader>cv :VimtexView<cr>
nnoremap <leader>ct :VimtexTocToggle<cr>
nnoremap <leader>cw :VimtexCountWords<cr>
