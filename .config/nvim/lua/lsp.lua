-- LSP (Neovim 0.5 built-in lsp client)

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noinsert,noselect'
vim.g.completion_matching_strategy_list = {'exact', 'substring', 'fuzzy'}

-- Mapping function
local function noremap(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Mappings
noremap('n', '<leader>k', '<cmd>lua vim.lsp.buf.hover()<CR>') -- Use K to show documentation in preview window
noremap('n', '<leader>ld', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')
noremap('i', '<silent><expr> <C-Space>', 'compe#complete()') -- map <c-space> to manually trigger completion
noremap('i', '<silent><expr> <CR>', 'compe#confirm("<CR>")')
noremap('i', '<silent><expr> <C-e>', 'compe#close("<C-e>")')

-- Language server setups
local lspconfig = require'lspconfig'

lspconfig.vimls.setup{ }

lspconfig.intelephense.setup{ }

lspconfig.pyls.setup{}


-- TsServer + Efm (for ESLint)
local eslint = {
  lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
  lintStdin = true,
  lintFormats = {"%f:%l:%c: %m"},
  lintIgnoreExitCode = true,
  formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
  formatStdin = true
}

lspconfig.tsserver.setup {
  on_attach = function(client)
    client.resolved_capabilities.document_formatting = false
  end
}

local function eslint_config_exists()
  local eslintrc = vim.fn.glob(".eslintrc*", 0, 1)

  if not vim.tbl_isempty(eslintrc) then
    return true
  end

  if vim.fn.filereadable("package.json") then
    if vim.fn.json_decode(vim.fn.readfile("package.json"))["eslintConfig"] then
      return true
    end
  end

  return false
end

lspconfig.efm.setup {
  on_attach = function(client)
    client.resolved_capabilities.document_formatting = true
    client.resolved_capabilities.goto_definition = false
  end,
  root_dir = function()
    if not eslint_config_exists() then
      return nil
    end
    return vim.fn.getcwd()
  end,
  settings = {
    languages = {
      javascript = {eslint},
      javascriptreact = {eslint},
      ["javascript.jsx"] = {eslint},
      typescript = {eslint},
      ["typescript.tsx"] = {eslint},
      typescriptreact = {eslint}
    }
  },
  filetypes = {
    "javascript",
    "javascriptreact",
    "javascript.jsx",
    "typescript",
    "typescript.tsx",
    "typescriptreact"
  },
}


--require('nlua.lsp.nvim').setup(lspconfig, {
--	-- Include globals you want to tell the LSP are real :)
--	globals = {
--    -- NodeMCU modules --
--          'file',
--          'gpio',
--          'http',
--          'net',
--          'node',
--          'sjson',
--          'softuart',
--          'tmr',
--          'uart',
--          'wifi'
--    ---------------------
--  }
--})

-- VsCode-like icons
require'lspkind'.init{ with_text = true  }

-- Auto completion (nvim-compe)
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;

  source = {
    path = true;
    buffer = true;
    calc = true;
    vsnip = false;
    ultisnips = true;
    nvim_lsp = true;
    nvim_lua = true;
    spell = true;
    tags = true;
    snippets_nvim = true;
    treesitter = true;
  };
}

