" -------------------------------------
" gitgutter
let g:gitgutter_max_signs = 1500
let g:gitgutter_diff_args = '-w'

" custom symbols for gitgutter
let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_modified = '~'
let g:gitgutter_sign_removed = '-'
let g:gitgutter_sign_removed_first_line = '^'
let g:gitgutter_sign_modified_removed = ':'

" -------------------------------------


" maping
map <Leader>gt :GitGutterToggle<cr>
nmap gn <Plug>(GitGutterNextHunk)
nmap gN <Plug>(GitGutterPrevHunk)
nmap gs <Plug>(GitGutterStageHunk)
nmap gu <Plug>(GitGutterUndoHunk)
nmap gp <Plug>(GitGutterPreviewHunk)
