vim.opt.swapfile = false

vim.opt.number = true
vim.opt.relativenumber = true

vim.cmd("filetype plugin indent on")

vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2

vim.cmd.colorscheme("habamax")

vim.cmd([[
let @t = "/int main\<CR>Ovoid solve() {\<ESC>o}\<Esc>/int main()\<CR>o int ttt; cin >> ttt;\<CR>while (ttt--) solve();\<Esc>/void solve()\<CR>o"
]])
