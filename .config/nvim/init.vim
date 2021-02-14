" ┓ ┳o┏┏┓┳━┓┏━┓
" ┃┏┛┃┃┃┃ ┃┳┛┃
" ┗┛ ┇┛ ┇┇┗┛┗━┛

let configs = [
\  'general',
\  'commands',
\  'plugins',
\]

for file in configs
  let x = expand('~/.config/nvim/config/'.file.'.vim')
  if filereadable(x)
    execute 'source' x
  endif
endfor


lua <<EOF
require('lsp')				-- lua/nvim-treesitter.lua
require('nvim-treesitter')	-- lua/nvim-treesitter.lua
EOF
