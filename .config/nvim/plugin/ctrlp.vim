" -------------------------------------
" CtrlP
let g:ctrlp_show_hidden = 1
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_map = '<c-p>'
map <C-i> :CtrlPBufTag<cr>
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

" -------------------------------------
