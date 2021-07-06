" -------------------------------------
" Start screen (startify)
let g:ascii = [
	\ '    ████     ██                           ██            ',
	\ '   ░██░██   ░██                          ░░             ',
	\ '   ░██░░██  ░██  █████   ██████  ██    ██ ██ ██████████ ',
	\ '   ░██ ░░██ ░██ ██░░░██ ██░░░░██░██   ░██░██░░██░░██░░██',
	\ '   ░██  ░░██░██░███████░██   ░██░░██ ░██ ░██ ░██ ░██ ░██',
	\ '   ░██   ░░████░██░░░░ ░██   ░██ ░░████  ░██ ░██ ░██ ░██',
	\ '   ░██    ░░███░░██████░░██████   ░░██   ░██ ███ ░██ ░██',
	\ '   ░░      ░░░  ░░░░░░  ░░░░░░     ░░    ░░ ░░░  ░░  ░░ ',
	\]
let g:startify_custom_header = g:ascii

let g:startify_lists = [
	\ { 'type': 'bookmarks', 'header': ['   Dotfiles'] },
	\ ]

let g:startify_bookmarks = [
    \ { 'xm': '~/.xmonad/xmonad.hs'},
    \ { 'xb': '~/.config/xmobar/xmobarrc'},
	\ { 'pp': '~/.config/polybar/config' },
	\ { 'al': '~/.config/alacritty/alacritty.yml' },
	\ { 'pi': '~/.config/picom.conf' },
	\ { 'tm': '~/.tmux.conf' },
	\ { 'nv': '~/.config/nvim' },
	\ { 'zs': '~/.zshrc'},
	\ { 'zz': '~/.config/zsh' },
	\ { 'sh': '~/.config/shell' },
	\ { 'zp': '~/.zprofile' },
	\ { 'bi': '~/.local/bin/' },
	\ { 'ra': '~/.config/ranger' },
	\ ]

" -------------------------------------
