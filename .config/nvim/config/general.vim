" ┏━┓┳━┓┏┓┓┳━┓┳━┓┳━┓┳
" ┃ ┳┣━ ┃┃┃┣━ ┃┳┛┃━┫┃
" ┇━┛┻━┛┇┗┛┻━┛┇┗┛┛ ┇┇━┛

" minimal number to keep above/below the cursor
set scrolloff=5

" enable auto indentation
set autoindent
set smartindent

" coffee pasta
set clipboard=unnamedplus

" use indents of 4 spaces
set shiftwidth=4
set shiftround

" tabs are tabs
set noexpandtab

" an indentation every four columns
set tabstop=4

" case insensitive search (until you use a capital)
set ignorecase
set smartcase
set infercase

" Give more space for displaying messages.
set cmdheight=2

" maintain undo history between sessions
set undofile
set nobackup
set noswapfile
set undodir=~/.config/nvim/cache/undo
"set backupdir=~/.config/nvim/cache/backups
"set directory=~/.config/nvim/cache/swaps

" Enables project specific configs:
" The current directory is searched for two files.
" The first that exists is used, the others are ignored.
"	-  The file ".nvimrc"
"	-  The file ".exrc"
set exrc

" plebs mode
set mouse=a

set tabpagemax=15

" show tabline only when more than one tab is present
set showtabline=1

"  split
set splitright
set splitbelow

set updatetime=100

" leader key
let mapleader = ' '

" encoding
scriptencoding utf-8
set encoding=utf8

" show matching brackets/parenthesis
set showmatch
set matchtime=3

" disable startup message
set shortmess+=I

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" hide buffers, not close them
set hidden

" True colors for colorschemes
set termguicolors

" Gui font for gvim and similars
"set guifont=DroidSansMono\ Nerd\ Font\ 11

" syntax highlighting
syntax enable
set synmaxcol=512
filetype plugin indent on
let g:vimsyn_embed = 'l' " Lua syntax highlighting inside .vim files

" autocompletion menu
set pumheight=10

" Statusline
set laststatus=2
set noruler
set showcmd
set noshowmode

" no folding
"set nofoldenable
"set foldlevel=99
"set foldminlines=99
"set foldlevelstart=99

" line wrapping
set nowrap

" show line numbers
set number relativenumber
set numberwidth=1

" show invisibles
set list
set listchars=
set listchars+=tab:·\
set listchars+=trail:·
set listchars+=extends:»
set listchars+=precedes:«
set listchars+=nbsp:░

" split style
" set fillchars=vert:▒

" highlight current line
augroup CursorLine
	au!
	"au VimEnter,WinEnter,BufWinEnter * setlocal cursorcolumn
	au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	au WinLeave * setlocal nocursorline
augroup END

" fuzzy find
set path+=**
" lazy file name tab completion
set wildmode=longest,list,full
set wildmenu
set wildignorecase
" ignore files vim doesnt use
set wildignore+=.git,.hg,.svn
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.rbc,*.class
set wildignore+=*.ai,*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png,*.psd,*.webp
set wildignore+=*.avi,*.divx,*.mp4,*.webm,*.mov,*.m2ts,*.mkv,*.vob,*.mpg,*.mpeg
set wildignore+=*.mp3,*.oga,*.ogg,*.wav,*.flac
set wildignore+=*.eot,*.otf,*.ttf,*.woff
set wildignore+=*.doc,*.pdf,*.cbr,*.cbz
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz,*.kgb
set wildignore+=*.swp,.lock,.DS_Store,._*

" language-specific
augroup langindentation
	autocmd Filetype c setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype cpp setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype css setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype typescript setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype html setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype json setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype scss setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype php setlocal tabstop=2 shiftwidth=2 softtabstop=2
	autocmd Filetype yaml setlocal tabstop=2 shiftwidth=2 softtabstop=2
augroup END


" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif
