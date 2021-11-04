"" Go
augroup filetype_go
	autocmd!
	autocmd FileType go nnoremap <leader>cr :GoRun<cr>
	autocmd FileType go nnoremap <leader>cb :GoBuild<cr>
	autocmd FileType go nnoremap <leader>ce :GoDescribe<cr>
	autocmd FileType go nnoremap <leader>ce :GoDescribe<cr>
	autocmd FileType go nnoremap <leader>co :GoDoc<cr>
	autocmd FileType go nnoremap <leader>cl :GoDocBrowser<cr>
	" change
	autocmd FileType go nnoremap <leader>cn :GoRename<cr>
	" goto
	autocmd FileType go nnoremap <leader>cp :GoPointsTo<cr>
	autocmd FileType go nnoremap <leader>cfr :GoReferrers<cr>
	" calls
	autocmd FileType go nnoremap <leader>cfc :GoCallers<cr>
	autocmd FileType go nnoremap <leader>cfe :GoCallees<cr>
	" tests
	autocmd FileType go nnoremap <leader>ct :GoTest<cr>
	autocmd FileType go nnoremap <leader>ctf :GoTestFunc<cr>
	autocmd FileType go nnoremap <leader>ctc :GoTestCompile<cr>
	" debug
	autocmd FileType go nnoremap <leader>cds :GoDebugStart<cr>
	autocmd FileType go nnoremap <leader>cdt :GoDebugTestFunc<cr>
	autocmd FileType go nnoremap <leader>cdb :GoDebugBreakpoint<cr>
	autocmd FileType go nnoremap <leader>cdc :GoDebugContinue<cr>
	autocmd FileType go nnoremap <leader>cdq :GoDebugStop<cr>
augroup END
