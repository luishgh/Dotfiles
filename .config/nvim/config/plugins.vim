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

" programming
"Plug 'neoclide/coc.nvim', {'branch': 'release'} 	" Autocompletion (LSP client) (use if nvim-slp can't be used)
Plug 'neovim/nvim-lspconfig' 						" Neovim 0.5 built-in lsp client)
Plug 'hrsh7th/nvim-compe' 							" Auto completion plugin for nvim lsp client
Plug 'onsails/lspkind-nvim' 						" Adds vscode-like icons to neovim built-in lsp
Plug 'sbdchd/neoformat'								" A (Neo)vim plugin for formatting code.

" highlighting (stable)
"Plug 'HerringtonDarkholme/yats.vim' " TS Syntax highlighting
"Plug 'neovimhaskell/haskell-vim' 	" Haskell Syntax highlighting
"Plug 'mxw/vim-jsx' 					" To highlight JSX
"Plug 'pangloss/vim-javascript' 		" To highlight JS
"Plug 'kevinoid/vim-jsonc' 			" Comment highlighting on config JSON files
"Plug 'rust-lang/rust.vim'			" Rust file detection, syntax highlighting, formatting, Syntastic integration, and more

" highlighting based on tree-sitter (Neovim 0.5 required)
Plug 'nvim-treesitter/nvim-treesitter', {'do': 'TSUpdate'} 	" We recommend updating the parsers on update
Plug 'nvim-treesitter/playground' 							" View treesitter information directly in Neovim

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'
Plug 'sheerun/vim-polyglot'
Plug 'mattn/emmet-vim'

" stylize
Plug 'lilydjwg/colorizer'
"Plug 'preservim/nerdtree' " File explorer
"Plug 'Xuyuanp/nerdtree-git-plugin' " Git plugin for NERDTree
Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'} "File Manager for Neovim, Better than NERDTree.
Plug 'isa/vim-matchit'
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' } " Zen mode
Plug 'junegunn/limelight.vim', { 'on': 'Goyo' } " Complements zen mode
Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }

" colorschemes
Plug 'gruvbox-community/gruvbox'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'

" features
Plug 'mhinz/vim-startify' " Home screen
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'matze/vim-move'
Plug 'simeji/winresizer'
Plug 'preservim/nerdcommenter' " Shortcuts for commenting
Plug 'christoomey/vim-tmux-navigator'
Plug 'vim-airline/vim-airline' " Statusline and tabline
Plug 'vim-airline/vim-airline-themes' " Themes for vim-airline
Plug 'tpope/vim-fugitive' "The premier Vim plugin for Git

" Fuzzy finder (Neovim Nightly (0.5) is required for telescope.nvim to work.)
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
" Plug 'ctrlpvim/ctrlp.vim' " fuzzy find files (use if telescope can't be used)

" Custom plugins
Plug 'ryanoasis/vim-devicons' " Devicons plugin
"Plug 'tiagofumo/vim-nerdtree-syntax-highlight' " Extra syntax highlighting for NERDTree

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
	autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
	autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
	autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
	autocmd FileType php setlocal omnifunc=phpcomplete#CompletePHP
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
"  NERDCommenter
" Comment selection
nmap <leader>c <plug>NERDCommenterToggle

" Comments configs
" Create default mappings
let g:NERDCreateDefaultMappings = 1
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" -------------------------------------


" -------------------------------------
" vim-airline
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

" -------------------------------------


" -------------------------------------
" CHADTree
nnoremap <leader>oe <cmd>CHADopen<cr>

" -------------------------------------


" -------------------------------------
" Colorscheme
" source $HOME/.config/nvim/config/themes/dracula.vim
colorscheme nord

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


" -------------------------------------
" LSP (Neovim 0.5 built-in lsp client)
set completeopt=menuone,noinsert,noselect " Set completeopt to have a better completion experience
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']

" Use K to show documentation in preview window
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <leader>ld <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>

" Language server setups
lua <<EOF
  local lspconfig = require"lspconfig"

  lspconfig.tsserver.setup { }

  lspconfig.vimls.setup{ }

  lspconfig.intelephense.setup{ }

  local system_name
  if vim.fn.has("mac") == 1 then
    system_name = "macOS"
  elseif vim.fn.has("unix") == 1 then
    system_name = "Linux"
  elseif vim.fn.has('win32') == 1 then
    system_name = "Windows"
  else
    print("Unsupported system for sumneko")
  end

  -- set the path to the sumneko installation
  local sumneko_root_path = '/home/luishgh/lua-language-server'
  local sumneko_binary = sumneko_root_path.."/bin/"..system_name.."/lua-language-server"

  lspconfig.sumneko_lua.setup {
    cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
    settings = {
      Lua = {
        runtime = {
          -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
          version = 'Lua 5.1.5',
          -- Setup your lua path
          path = vim.split(package.path, ';'),
        },
        diagnostics = {
          globals = {
		  	'vim', -- Get the language server to recognize the `vim` global

			-- NodeMCU modules --
			'file',
			'gpio',
			'http',
			'net',
			'node',
			'sjson',
			'softuart',
			'tmr',
			'uart',
			'wifi'
			---------------------

		  },
        },
        workspace = {
          -- Make the server aware of Neovim runtime files
          library = {
            [vim.fn.expand('$VIMRUNTIME/lua')] = true,
            [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
          },
        },
      },
    },
  }
EOF

" VsCode-like icons
lua require'lspkind'.init{ with_text = true  }

" Auto completion (nvim-compe)
lua <<EOF
  require'compe'.setup {
    enabled = true;
    autocomplete = true;
    debug = false;
    min_length = 1;
    preselect = 'enable';
    throttle_time = 80;
    source_timeout = 200;
    incomplete_delay = 400;
    max_abbr_width = 100;
    max_kind_width = 100;
    max_menu_width = 100;

    source = {
      path = true;
      buffer = true;
      calc = true;
      vsnip = true;
      nvim_lsp = true;
      nvim_lua = true;
      spell = true;
      tags = true;
      snippets_nvim = true;
      treesitter = true;
    };
  }
EOF

"map <c-space> to manually trigger completion
inoremap <silent><expr> <C-Space> compe#complete()

" Other completion mappings
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')

" -------------------------------------


" -------------------------------------
" nvim-treesitter (Neovim 0.5 highlighting)
lua <<EOF
  require'nvim-treesitter.configs'.setup {
    highlight = {
      enable = true,              -- false will disable the whole extension
      -- disable = {},  			-- list of language that will be disabled
    },
    indent = {
      enable = true,
    },
    playground = {
	  enable = true,
	  updatetime = 25
    },
  }
EOF

" -------------------------------------
