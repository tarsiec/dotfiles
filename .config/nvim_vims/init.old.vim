" NEOVIM CONFIGURATION: THIS TIME MINIMAL EDITION
call plug#begin("~/.config/nvim/plugins")
	"" COMPLETION
	Plug 'Shougo/deoplete.nvim'
	Plug 'roxma/nvim-yarp'
	Plug 'roxma/vim-hug-neovim-rpc'
	"" LINTER
	Plug 'dense-analysis/ale'
	"" LANGUAGES
	Plug 'fatih/vim-go'
	Plug 'neovimhaskell/haskell-vim'
    Plug 'cespare/vim-toml'
	" Plug 'alx741/vim-hindent'
	"" TAGS
	Plug 'majutsushi/tagbar'
	Plug 'ludovicchabant/vim-gutentags'
	"" GIT
	Plug 'tpope/vim-fugitive'
	"" LOW-LEVEL IMPROVEMENTS
	Plug 'tpope/vim-surround'
	Plug 'tpope/vim-repeat'
	Plug 'tpope/vim-commentary'
	Plug 'cohama/lexima.vim'
	Plug 'junegunn/fzf'
	"" UI
    Plug 'ncm2/float-preview.nvim'
	Plug 'junegunn/goyo.vim'
	Plug 'voldikss/vim-floaterm'
	" Plug 'preservim/nerdtree'
	Plug 'itchyny/lightline.vim'
	" Plug 'vim-airline/vim-airline'
    " Plug 'GeorgeHJ/tmux-airline'
	" Plug 'vim-airline/vim-airline-themes'
	"" SNIPPETS
	Plug 'sirver/ultisnips'
	Plug 'honza/vim-snippets'
	"" THEMEING
	Plug 'chriskempson/base16-vim'
    Plug 'jnurmine/Zenburn'
call plug#end()


""" PREFACE
	"" Set leader keys
		let mapleader = " "
		let maplocalleader = "\\"


	"" Make splits split right
		set splitbelow
		set splitright


	"" Highlight changes on commands
		" search
		set nohlsearch
		set incsearch
		set smartcase
		" substitute
		set inccommand=nosplit


	"" Scrolling
		" allow scrolling
		set mouse=nv
		" always below the cursor
		set scrolloff=1

	"" Indent/whitespace
		" indent
		set tabstop=4
		set shiftwidth=4
		set expandtab
		set autoindent
		set smartindent

		" whitespace
		set nolist
		set wildmenu

	"" Folding
		" setlocal foldignore=

	"" Finding files
		" search down into subfolders
		set path+=**

""" APPEARANCE
	"" Basic
		set termguicolors
		set cursorline
		set background=dark
		colorscheme base16-3024


	"" Transparency
		highlight LineNr ctermbg=NONE guibg=NONE
        au ColorScheme * hi Normal ctermbg=none guibg=none
        au ColorScheme myspecialcolors hi Normal ctermbg=red guibg=red



	"" Number bar
		set nu rnu

	"" Line
		set background=dark
		let g:lightline = { 'colorscheme': 'solarized' }
		let g:airline_theme='tomorrow'

		let g:airline_powerline_fonts = 1
		let g:airline#extensions#ale#enabled = 1

		" function! GitBranch()
		"   return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
		" endfunction

		" function! StatuslineGit()
		"   let l:branchname = GitBranch()
		"   return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
		" endfunction

		" set statusline=
		" set statusline+=%#PmenuSel#
		" set statusline+=%{StatuslineGit()}
		" set statusline+=%#LineNr#
		" set statusline+=\ %f
		" set statusline+=%m
		" set statusline+=%=
		" set statusline+=%#CursorColumn#
		" set statusline+=\ %y
		" set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
		" set statusline+=\[%{&fileformat}\]
		" set statusline+=\ %p%%
		" set statusline+=\ %l:%c
		" set statusline+=\

""" MAPPINGS
	"" Ease of life
		inoremap jk <esc>
		"
		"" Move position of lines
		"- up
		nnoremap - :m .-2<cr>==
		"- down
        nnoremap _ :m .+1<cr>==

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
		imap <M-l> <esc>:vertical resize +5<cr>i
		imap <M-h> <esc>:vertical resize -5<cr>i
		imap <M-j> <esc>:resize +5<cr>i
		imap <M-k> <esc>:resize -5<cr>i

		nnoremap <M-l> :vertical resize +5<cr>
		nnoremap <M-h> :vertical resize -5<cr>
		nnoremap <M-j> :resize +5<cr>
		nnoremap <M-k> :resize -5<cr>
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


	"" Git
		" - git commands in vim
		" main page
		nnoremap <leader>og :Git<cr>
		" add
		nnoremap <leader>ga :Git add 
		nnoremap <leader>gaa :Git add .<cr>
		" commmit
		nnoremap <leader>gc :Git commit<cr>
		" diff
		nnoremap <leader>gd :Git diff<cr>
		nnoremap <leader>gdt :Git difftool<cr>
		" log
		nnoremap <leader>gl :Git log<cr>
		" status
		nnoremap <leader>gs :Git status<cr>
		" blame
		nnoremap <leader>gb :Git blame<cr>
		" merge
		nnoremap <leader>gm :Git merge<cr>
		nnoremap <leader>gmt :Git mergetool<cr>
		" - higher level commands
		" edit files as they used to be
		nnoremap <leader>geb :Gedit HEAD~:%<left><left>
		nnoremap <leader>gesb :Gsplit HEAD~:%<left><left>
		" compare current and staged files
		nnoremap <leader>gds :Gdiffsplit 
		nnoremap <leader>gdsv :Gdiffsplit 
		" git grep
		nnoremap <leader>sg :Ggrep 
		" rename
		nnoremap <leader>gr :GRename 
		" open in browser
		nnoremap <leader>go :GBrowse<cr>
		" git rm
		nnoremap <leader>gk :GDelete 

""" UI
	"" Terminal
		nmap<leader>ot :FloatermToggle<cr>
		nmap<leader>oT :new<cr>:term<cr>

	"" Writing
		" Goyo
		nnoremap <leader>tw :Goyo<cr>

	"" Files
		" NERDTree
		let NERDTreeShowHidden=1
		nnoremap <leader>of :NERDTreeToggle<CR>
		nnoremap <C-f> :NERDTreeFind<CR>

		" Exit Vim if NERDTree is the only window left.
		autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
				\ quit | endif


	"" Tags
		nnoremap <leader>om :TagbarToggle<cr>

""" COMPLETION
	"" Autocompletion
		filetype plugin on
		let g:deoplete#enable_at_startup = 1
		set omnifunc=ale#completion#OmniFunc
		let g:ale_completion_enabled = 1
    set completeopt-=preview
    let g:float_preview#docked = 0
    let g:ale_floating_preview=1
    let g:ale_hover_to_floating_preview=1
    let g:ale_detail_to_floating_preview=1

""" LINTING
	"" Config
		" ALE
		" \	'haskell': ['hlint', 'hindent'],
		let g:ale_fixers = {
		\	'haskell': ['hlint','brittany'],
        \   'go':      ['goimports']
		\ }


		let g:ale_floating_window_border = ['│', '─', '╭', '╮', '╯', '╰']


		let g:ale_echo_msg_error_str = 'E'
		let g:ale_echo_msg_warning_str = 'W'
		let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

		let g:ale_lint_on_text_changed = 'never'
		let g:ale_lint_on_enter = 0
		let g:ale_fix_on_save = 1


	"" Mappings
		nmap <silent> [e <Plug>(ale_previous_wrap)
		nmap <silent> ]e <Plug>(ale_next_wrap)

		nnoremap <silent> gd :ALEGoToDefinition<cr>
		nnoremap <silent> gt :ALEGoToTypeDefinition<cr>
		nnoremap <silent> gr :ALEFindReferences<cr>
		nnoremap <silent> gk :ALEDocumentation<cr>
		nnoremap <silent> <f2> :ALERename<cr>
		nnoremap ge :ALERename<cr>
		nnoremap gs :ALESymbolSearch 
		" nnoremap <silent>  :ALEDocumentation<cr>

""" LANGUAGES
	"" Haskell
			" haskell-vim
			let g:haskell_enable_quantification = 1   " `forall`
			let g:haskell_enable_recursivedo = 1      " `mdo` and `rec`
			let g:haskell_enable_arrowsyntax = 1      " `proc`
			let g:haskell_enable_pattern_synonyms = 1 " `pattern`
			let g:haskell_enable_typeroles = 1        " type roles
			let g:haskell_enable_static_pointers = 1  " `static`
			let g:haskell_backpack = 1                " backpack keywords

			

			augroup config_hs
				autocmd FileType haskell set tabstop=2
				autocmd FileType haskell set shiftwidth=2
				autocmd FileType haskell setlocal foldmethod=syntax
			augroup END


	"" Commands
		let g:go_fmt_command="goimports"
		let g:go_auto_type_info = 1
	"" Mappings
		augroup filetype_hs
			" compile in terminal
			autocmd FileType haskell nnoremap <silent> <leader>cc :make<cr>
			autocmd FileType haskell nnoremap <silent> <leader>ce :copen<cr>
			" general
			autocmd FileType haskell nnoremap <silent> <leader>cr :make<cr>:!./Main<cr>
			autocmd FileType haskell nnoremap <silent> <leader>cx :FloatermNew --autoclose=0 ghc -o out % && ./out<cr>
			" repl
			autocmd FileType haskell nnoremap <silent> <leader>or :new<cr>:term ghci<cr>
			autocmd FileType haskell nnoremap <silent> <leader>oR :new<cr>:FloatermNew --autoclose=0 ghci %<cr>
			" change
			" autocmd FileType c nnoremap <silent> <leader>cf :
			" goto
			" calls
			" tests
			" debug
		augroup END

		augroup filetype_c
			" compile in terminal
			autocmd FileType c nnoremap <silent> <leader>cc :make<cr>
			autocmd FileType c nnoremap <silent> <leader>ce :copen<cr>
			" general
			autocmd FileType c nnoremap <silent> <leader>cr :make<cr>:./main<cr>
			autocmd FileType c nnoremap <silent> <leader>cx :FloatermNew --autoclose=0 gcc % && ./a.out<cr>
			" change
			" autocmd FileType c nnoremap <silent> <leader>cf :
			" goto
			" calls
			" tests
			" debug
		augroup END

		augroup filetype_go
			" compile in terminal
			autocmd FileType go	nnoremap <silent> <leader>cc :FloatermNew --autoclose=0 go run %<cr>
			" general
			autocmd FileType go nnoremap <leader>cr :GoRun<cr>
			autocmd FileType go nnoremap <leader>cb :GoBuild<cr>
			autocmd FileType go nnoremap <leader>ce :GoDescribe<cr>
			autocmd FileType go nnoremap <leader>ce :GoDescribe<cr>
			autocmd FileType go nnoremap <leader>co :GoDoc<cr>
			autocmd FileType go nnoremap <leader>cob :GoDocBrowser<cr>
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

""" SNIPPETS
	"" UltiSnips
		let g:UltiSnipsExpandTrigger="<tab>"
		let g:UltiSnipsJumpForwardTrigger="<tab>"
		let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
		let g:UltiSnipsEditSplit="horizontal"
