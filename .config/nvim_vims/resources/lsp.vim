"" Give more space for displaying messages.
set cmdheight=2

"" Having longer updatetime leads to poorer user experience
set updatetime=300

"" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

"" <c-space> triggers completion
inoremap <silent><expr> <c-space> coc#refresh()

"" Navigate diagnostics with ]/[g
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

"" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

"" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

"" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocAction('highlight')

" Symbol renaming.
nmap <leader>ar <Plug>(coc-rename)
" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>aq <Plug>(coc-fix-current)

"" Map function and class text objects
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

"" Remap <C-f> and <C-b> for scroll float windows/popups.
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

"" Use CTRL-S for selections ranges.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

"" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

"" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

"" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

"" Maps for formatting, folding, og
nnoremap <leader>af :Format<cr>
nnoremap <leader>az :Fold<cr>
nnoremap <leader>ao :OR<cr>

"" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>ld  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>le  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>lc  :<C-u>CocList commands<cr>
" Find symbol of current documentl
nnoremap <silent><nowait> <space>lo  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>ls  :<C-u>CocList -I symbols<cr>
" Do default action for next iteml
nnoremap <silent><nowait> <space>aj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>ak  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>lr  :<C-u>CocListResume<CR>

"" EXTENSIONS

let g:coc_global_extensions = [
			\ 'coc-go',
			\ 'coc-json',
			\ 'coc-marketplace',
			\ 'coc-pyright',
			\ 'coc-tsserver'
			\ ]
