lua << EOF
require('telescope').setup{ }
EOF


" -------------------------------------
" telescope.nvim

" Find files using lua functions
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <C-p> :lua require('telescope.builtin').git_files()<CR>
nnoremap <leader>fg :lua require('telescope.builtin').grep_string({ search = vim.fn.input("Grep For > ")})<CR>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>

" -------------------------------------
