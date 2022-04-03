" indentation
au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

" virtualenv
" py << EOF
" import os
" import sys
" if 'VIRTUAL_ENV' in os.environ:
" 	project_base_dir = os.environ['VIRTUAL_ENV']
" 	activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
" 	execfile(activate_this, dict(__file__=activate_this))
" EOF

" run python
nnoremap <leader>cc :!python %<cr>
