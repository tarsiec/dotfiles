"" Make Y behave like the rest of the capital actions
nnoremap Y yg_

"" Make commas, etc. a breakpoint in undo history
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap [ [<c-g>u
inoremap ? ?<c-g>u
inoremap ! !<c-g>u
inoremap ( (<c-g>u

"" Move text in different modes
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv
inoremap <c-j> <esc>:m .+1<cr>==<esc>i
inoremap <c-k> <esc>:m .-2<cr>==<esc>i
nnoremap <leader>K <esc>:m .-2<cr>==
nnoremap <leader>J <esc>:m .+1<cr>==

"" Exit insert mode
inoremap jk <esc>
"
"" Move position of lines
"- up
nnoremap - ddp
"- down
nnoremap _ ddkkp

nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>


"" Window /  Tab
" focus
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
nnoremap <c-k> <c-w>k
nnoremap <c-j> <c-w>j
" focus
nnoremap <c-y>h :tabr<cr>
nnoremap <c-y>j :tabl<cr>
nnoremap <c-y>k :tabp<cr>
nnoremap <c-y>l :tabn<cr>
" size
nnoremap <M-l> :vertical resize +5<cr>
nnoremap <M-h> :vertical resize -5<cr>
nnoremap <M-j> :resize +5<cr>
nnoremap <M-k> :resize -5<cr>

imap <M-l> <esc>:vertical resize +5<cr>i
imap <M-h> <esc>:vertical resize -5<cr>i
imap <M-j> <esc>:resize +5<cr>
imap <M-k> <esc>:resize -5<cr>i

" terminal
tnoremap <esc> <c-\><c-n>
tnoremap <c-[> <c-\><c-n>
tnoremap jk <c-\><c-n>
tnoremap <c-h> <c-\><c-n><c-w>h
tnoremap <c-l> <c-\><c-n><c-w>l
tnoremap <c-k> <c-\><c-n><c-w>k
tnoremap <c-j> <c-\><c-n><c-w>j


"" Yanking and pasting
nnoremap yu yypk
nnoremap yo yykpj


"" Inside-stuff
" - inside next/prev parens
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! f)vi(<cr>
" - inside next/prev angle brackets
onoremap in< :<c-u>normal! f<vi<<cr>
onoremap il< :<c-u>normal! f>vi<<cr>
" - inside next/prev square brackets
onoremap in[ :<c-u>normal! f[vi{<cr>
onoremap il[ :<c-u>normal! f]vi{<cr>
" - inside next/prev curly brackets
onoremap in{ :<c-u>normal! f{vi{<cr>
onoremap il{ :<c-u>normal! f}vi{<cr>
" - inside next/prev parens
onoremap in" :<c-u>normal! f"vi"<cr>
onoremap il" :<c-u>normal! f"vi"<cr>


"" Spelling
nnoremap <leader>ts :set spell!<cr>
inoremap <C-l> <c-g>u<esc>[s1z=`]a<c-g>u
