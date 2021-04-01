lua << EOF
require('telescope').setup{
	defaults = {
		file_sorter = require'telescope.sorters'.get_fzy_sorter,
	},
	extensions = {
		fzy_native = {
			override_generic_sorter = false,
			override_file_sorter = true,
		}
	}
}

require'telescope'.load_extension'fzy_native'
EOF


" -------------------------------------
" telescope.nvim

" Find files using lua functions
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<CR>
nnoremap <C-p> 		<cmd>lua require('telescope.builtin').git_files()<CR>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').grep_string({ search = vim.fn.input("Grep For > ")})<CR>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<CR>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<CR>

" -------------------------------------
