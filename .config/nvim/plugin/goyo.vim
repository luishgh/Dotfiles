" -------------------------------------
" GoYo - distraction free writing mode (Zen mode)
let g:limelight_conceal_ctermfg = 8
function! s:goyo_enter()
	Limelight
	noremap ZZ :Goyo\|x!<cr>
	noremap ZQ :Goyo\|q!<cr>
	silent !tmux set status off
	silent !tmux list-panes -F '\#F' | grep -q Z | tmux resize-pane -Z
	set noshowmode
	set noshowcmd
	set wrap
	set scrolloff=999
endfunction

function! s:goyo_leave()
	Limelight!
	unmap ZZ
	unmap ZQ
	silent !tmux set status on
	silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
	set showmode
	set showcmd
	set nowrap
	set scrolloff=0
endfunction

augroup goyoactions
	au!
	autocmd! User GoyoEnter nested call <SID>goyo_enter()
	autocmd! User GoyoLeave nested call <SID>goyo_leave()
augroup end

" Keybind to enter Zen mode
map <leader>z :Goyo<cr>

" Enable Goyo by default for mutt writting
augroup muttwrite
	au!
	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<cr>
	autocmd BufRead,BufNewFile /tmp/neomutt* map ZQ :Goyo\|q!<cr>
augroup end
