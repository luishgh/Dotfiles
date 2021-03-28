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
Plug 'neovim/nvim-lspconfig' 						" Neovim 0.5 built-in lsp client configuration collection
Plug 'hrsh7th/nvim-compe' 							" Auto completion plugin for nvim lsp client
Plug 'onsails/lspkind-nvim' 						" Adds vscode-like icons to neovim built-in lsp
Plug 'nvim-lua/lsp_extensions.nvim' 				" Extensions to built-in LSP
Plug 'kosayoda/nvim-lightbulb' 						" VSCode 💡 for neovim's built-in LSP
" Plug 'tjdevries/nlua.nvim' 							" Install sumneko lua.

" highlighting (stable)
" Plug 'HerringtonDarkholme/yats.vim' 	" TS Syntax highlighting
" Plug 'neovimhaskell/haskell-vim' 		" Haskell Syntax highlighting
" Plug 'mxw/vim-jsx' 						" To highlight JSX
Plug 'pangloss/vim-javascript' 			" To highlight JS
Plug 'kevinoid/vim-jsonc' 				" Comment highlighting on config JSON files
Plug 'rust-lang/rust.vim'				" Rust file detection, syntax highlighting, formatting, Syntastic integration, and more
Plug 'euclidianAce/BetterLua.vim'		" Better Lua syntax highlighting
" Plug 'sheerun/vim-polyglot'

" highlighting based on tree-sitter (Neovim 0.5 required)
" Plug 'nvim-treesitter/nvim-treesitter', {'do': 'TSUpdate'} 	" We recommend updating the parsers on update
" Plug 'nvim-treesitter/playground' 							" View treesitter information directly in Neovim

" Snippets
" Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'
" Plug 'mattn/emmet-vim'

" stylize
Plug 'RRethy/vim-hexokinase', {'do': 'make hexokinase', 'on': 'HexokinaseToggle'} " The fastest (Neo)Vim plugin for asynchronously displaying the colours in the file
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps', 'on': 'CHADopen' } "File Manager for Neovim, Better than NERDTree.
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' } " Zen mode
Plug 'junegunn/limelight.vim', { 'on': 'Goyo' } " Complements zen mode
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }

" colorschemes
Plug 'gruvbox-community/gruvbox'
" Plug 'dracula/vim', { 'as': 'dracula' }
" Plug 'arcticicestudio/nord-vim'

" Tim Pope masterpieces
Plug 'tpope/vim-surround' 	" Provides mappings to easily delete, change and add such surroundings in pairs
Plug 'tpope/vim-commentary' " Shortcuts for commenting
Plug 'tpope/vim-fugitive' 	"The premier Vim plugin for Git

" features
Plug 'mhinz/vim-startify' " Home screen
Plug 'matze/vim-move'
Plug 'airblade/vim-gitgutter'
Plug 'sbdchd/neoformat', { 'on': 'Neoformat'} " A (Neo)vim plugin for formatting code.
" Plug 'junegunn/gv.vim' " A git commit browser
" Plug 'simeji/winresizer'
" Plug 'christoomey/vim-tmux-navigator'
" Plug 'vim-airline/vim-airline' " Statusline and tabline
" Plug 'vim-airline/vim-airline-themes' " Themes for vim-airline

" Fuzzy finder (Neovim Nightly (0.5) is required for telescope.nvim to work.)
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'kyazdani42/nvim-web-devicons' " A lua fork of vim-devicons, provides the same icons as well as colors for each icon.

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


" emmet settings
let g:user_emmet_mode='a'
let g:user_emmet_leader_key=','
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

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

" if you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit='vertical'


" UltiSnips
let g:UltiSnipsExpandTrigger='<C-z>'
let g:UltiSnipsJumpForwardTrigger='<C-s>'
let g:UltiSnipsJumpBackwardTrigger='<C-g>'


" -------------------------------------
" UndoTree
nnoremap <F5> :UndotreeToggle<cr>

" -------------------------------------


" -------------------------------------
" vim-airline
" let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_extensions = []

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
" vim-devicons
if exists("g:loaded_webdevicons")
  call webdevicons#refresh()
endif

" Settings that should come by default
let g:webdevicons_enable = 1 " loading the plugin
let g:webdevicons_enable_nerdtree = 1 " adding the flags to NERDTree
let g:webdevicons_enable_airline_tabline = 1 " adding to vim-airline's tabline
let g:webdevicons_enable_airline_statusline = 1
let g:webdevicons_enable_ctrlp = 1
let g:webdevicons_enable_startify = 1
let g:WebDevIconsUnicodeGlyphDoubleWidth = 1
let g:WebDevIconsNerdTreeGitPluginForceVAlign = 1
let g:WebDevIconsNerdTreeAfterGlyphPadding = '  '
let g:webdevicons_conceal_nerdtree_brackets = 1

" Custom extension symbols
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {} "
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['js'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['ts'] = 'ﯤ'
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['css'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['html'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['json'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['md'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['sql'] = ''

" -------------------------------------


" vim.lsp
augroup luaautocommands

	" This prevents having the autocommands defined twice (e.g., after sourcing the vimrc file again).
	au!

	" Show diagnostic popup on cursor hold
	autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()

	" Enable type inlay hints
	autocmd CursorMoved,InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost *
	\ lua require'lsp_extensions'.inlay_hints{ prefix = '', highlight = "Comment", enabled = {"TypeHint", "ChainingHint", "ParameterHint"} }

augroup end

" -------------------------------------


" -------------------------------------
" nvim-lightbulb
lua << EOF
require'nvim-lightbulb'.update_lightbulb {
    sign = {
        enabled = true,
        -- Priority of the gutter sign
        priority = 10,
    },
    float = {
        enabled = false,
        -- Text to show in the popup float
        text = "💡",
        -- Available keys for window options:
        -- - height     of floating window
        -- - width      of floating window
        -- - wrap_at    character to wrap at for computing height
        -- - max_width  maximal width of floating window
        -- - max_height maximal height of floating window
        -- - pad_left   number of columns to pad contents at left
        -- - pad_right  number of columns to pad contents at right
        -- - pad_top    number of lines to pad contents at top
        -- - pad_bottom number of lines to pad contents at bottom
        -- - offset_x   x-axis offset of the floating window
        -- - offset_y   y-axis offset of the floating window
        -- - anchor     corner of float to place at the cursor (NW, NE, SW, SE)
        -- - winblend   transparency of the window (0-100)
        win_opts = {},
    },
    virtual_text = {
        enabled = false,
        -- Text to show at virtual text
        text = "💡",
    }
}
EOF

" -------------------------------------

" vim-hexokinase
let g:Hexokinase_highlighters = ['virtual']
