"" Allow 256 colorspace
let base16colorspace=256

set termguicolors

set background=dark

"" Find color schemes
nnoremap <leader>tc :Telescope colorscheme<cr>

"" Highlight the row
set nocursorcolumn cursorline

"" Set colorscheme
colorscheme nord
" colorscheme base16-tomorrow-night

"" Set airline theme
let g:airline_theme="nord"
" let g:airline_theme="base16_tomorrow_night"
" let g:airline_theme="base16_3024"

"" Transparency
highlight Normal ctermbg=NONE guibg=NONE
highlight LineNr ctermbg=NONE guibg=NONE

"" Airline config
" air-line
let g:airline_powerline_fonts = 1
