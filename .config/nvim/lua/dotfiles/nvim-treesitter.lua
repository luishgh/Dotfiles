-- nvim-treesitter (Neovim 0.5 highlighting)
require'nvim-treesitter.configs'.setup {
  -- ensure_installed = 'maintained', -- use all maintained languages modules
  highlight = {
    enable = true,      -- false will disable the whole extension
    disable = {'lua'},  -- list of language that will be disabled
  },
  indent = {
    enable = true,
  },
  playground = {
    enable = true,
    updatetime = 25
  },
}
