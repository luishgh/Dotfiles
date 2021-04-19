" ┳━┓┳  ┳ ┓┏━┓o┏┓┓┓━┓
" ┃━┛┃  ┃ ┃┃ ┳┃┃┃┃┗━┓
" ┇  ┇━┛┇━┛┇━┛┇┇┗┛━━┛


" setup vim-plug
if empty(glob('~/.config/nvim/autoload/plug.vim'))
	echo 'Downloading junegunn/vim-plug to manage plugins...'
	silent call system('mkdir -p ~/.config/nvim/{autoload,bundle,cache/{cache,undo,backups,swaps}}')
	silent call system('pip3 install --user pynvim msgpack')
	silent call system('curl -flo ~/.config/nvim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim')
	execute 'source  ~/.config/nvim/autoload/plug.vim'
	augroup plugsetup
		au!
		autocmd VimEnter * PlugInstall
	augroup end
endif


" -------------------------------------
" Plugins (vim-plug)
call plug#begin('~/.config/nvim/bundle')

" programming (stable)
" Plug 'neoclide/coc.nvim', {'branch': 'release'} 	" Autocompletion (LSP client) (use if nvim-lsp can't be used)

" programming (Neovim 0.5)
Plug 'neovim/nvim-lspconfig' 		" Neovim 0.5 built-in lsp client configuration collection
Plug 'hrsh7th/nvim-compe' 			" Auto completion plugin for nvim lsp client
Plug 'onsails/lspkind-nvim' 		" Adds vscode-like icons to neovim built-in lsp
Plug 'nvim-lua/lsp_extensions.nvim' " Extensions to built-in LSP
Plug 'kosayoda/nvim-lightbulb' 		" VSCode 💡 for neovim's built-in LSP
Plug 'tjdevries/nlua.nvim' 			" Install sumneko lua.
" Plug 'nvim-lua/lsp-status.nvim'

" language plugins
Plug 'rust-lang/rust.vim'				" Rust file detection, syntax highlighting, formatting, Syntastic integration, and more

" highlighting (stable)
" Plug 'HerringtonDarkholme/yats.vim' 	" TS Syntax highlighting
" Plug 'neovimhaskell/haskell-vim' 		" Haskell Syntax highlighting
" Plug 'mxw/vim-jsx' 						" To highlight JSX
" Plug 'pangloss/vim-javascript' 			" To highlight JS
" Plug 'kevinoid/vim-jsonc' 				" Comment highlighting on config JSON files
" Plug 'euclidianAce/BetterLua.vim'		" Better Lua syntax highlighting

" highlighting based on tree-sitter (Neovim 0.5 required)
Plug 'nvim-treesitter/nvim-treesitter', {'do': 'TSUpdate'} 	" We recommend updating the parsers on update
" Plug 'nvim-treesitter/playground' 							" View treesitter information directly in Neovim

" Snippets
Plug 'hrsh7th/vim-vsnip'            " VSCode(LSP)'s snippet feature in vim.
Plug 'rafamadriz/friendly-snippets' " Snippets collection for a set of different programming languages for faster development.

" stylize
Plug 'RRethy/vim-hexokinase', {'do': 'make hexokinase', 'on': 'HexokinaseToggle'} " The fastest (Neo)Vim plugin for asynchronously displaying the colours in the file
Plug 'mhinz/vim-startify' " Home screen
Plug 'glepnir/galaxyline.nvim' , {'branch': 'main'}
Plug 'kyazdani42/nvim-web-devicons' " A lua fork of vim-devicons, provides the same icons as well as colors for each icon.

" colorschemes
Plug 'gruvbox-community/gruvbox' " A bright theme with pastel 'retro groove' colors and light/dark mode switching
Plug 'dracula/vim', { 'as': 'dracula' }
" Plug 'arcticicestudio/nord-vim'

" Tim Pope masterpieces
Plug 'tpope/vim-surround' 	" Provides mappings to easily delete, change and add such surroundings in pairs
Plug 'tpope/vim-commentary' " Shortcuts for commenting
Plug 'tpope/vim-fugitive' 	" The premier Vim plugin for Git

" features
Plug 'matze/vim-move' " Moves lines and selections in a more visual manner
Plug 'airblade/vim-gitgutter' " Shows a git diff in the sign column.
Plug 'sbdchd/neoformat', { 'on': 'Neoformat'} " A (Neo)vim plugin for formatting code.
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps', 'on': 'CHADopen' } "File Manager for Neovim, Better than NERDTree.
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' } " Zen mode
Plug 'junegunn/limelight.vim', { 'on': 'Goyo' } " Complements zen mode
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' } " Visualizes undo history and makes it easier to browse and switch between different undo branches.
" Plug 'junegunn/gv.vim' " A git commit browser
" Plug 'simeji/winresizer'
" Plug 'christoomey/vim-tmux-navigator'

" Fuzzy finder (Neovim Nightly (0.5) is required for telescope.nvim to work.)
Plug 'nvim-lua/popup.nvim' " [WIP] An implementation of the Popup API from vim in Neovim.
Plug 'nvim-lua/plenary.nvim' " All the lua functions I don't want to write twice.
Plug 'nvim-telescope/telescope.nvim' " A highly extendable fuzzy finder over lists
Plug 'nvim-telescope/telescope-fzy-native.nvim' " FZY style sorter for telescope.nvim that is compiled

" Plug 'ctrlpvim/ctrlp.vim' " fuzzy find files (use if telescope can't be used)

" Custom plugins
" Plug 'ryanoasis/vim-devicons' " Devicons plugin

call plug#end()


" -------------------------------------

" python paths, needed for virtualenvs
let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog = '/usr/bin/python2'

" syntax highlighting
let g:python_highlight_all = 1

" ignore whitspace
let g:better_whitespace_filetypes_blacklist=['<filetype1>', '<filetype2>', '<etc>',  'diff', 'gitcommit', 'unite', 'qf', 'help']

" -------------------------------------


" resize split
let g:winresizer_horiz_resize = 1
let g:winresizer_vert_resize = 2


" Removed because completion gets confusing lol
" completion with tab
" inoremap <expr><silent><tab> pumvisible() ? "\<c-n>" : "\<tab>"
" inoremap <expr><silent><S-tab> pumvisible() ? "\<c-p>" : "\<tab>"

"inoremap <tab> <c-n>
"inoremap <S-tab> <c-p>

" real tab
inoremap <A-tab> <tab>

" omnifuncs
augroup omnifuncs
	au!
	autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
	autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
	" autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
	" autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
	autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
	" autocmd FileType php setlocal omnifunc=phpcomplete#CompletePHP
augroup end


" disable preview
set completeopt-=preview


" -------------------------------------
" UndoTree
nnoremap <F5> :UndotreeToggle<cr>

" -------------------------------------


" -------------------------------------
" CHADTree
nnoremap <leader>oe <cmd>CHADopen<cr>

" -------------------------------------


" -------------------------------------
" Colorscheme
" source $HOME/.config/nvim/config/themes/dracula.vim
let g:gruvbox_contrast_dark = 'hard'
colorscheme gruvbox

" -------------------------------------


" -------------------------------------
" vim-hexokinase
let g:Hexokinase_highlighters = ['virtual']

" -------------------------------------


" -------------------------------------
" Mappings for snippets for filetypes without LSP servers
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR> compe#confirm("<CR>")
inoremap <silent><expr> <C-e> compe#close("<C-e>")

" -------------------------------------
