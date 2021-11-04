"" The barebones
set wildmenu

"" Go down folders when looking for files
set path+=**

"" Spellcheck
set spelllang=es,gl,en_gb

"" Autocomplete parenthesis pairs
lua << EOF
-- set up "autopairs"
require('nvim-autopairs').setup{}
EOF

"" Snippets

