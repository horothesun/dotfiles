-- ~/.config/nvim/after/ftplugin/python.lua

local lsp = require("config.lsp")

lsp.setup()

-- TODO: implement!!! 🔥🔥🔥
-- require("config.python_snippets").setup()

vim.lsp.config['ruff'] = {
  cmd = { 'ruff', 'server' },
  filetypes = { 'python' },
  root_markers = { 'pyproject.toml', 'ruff.toml', '.ruff.toml', '.git' },
  settings = {},
}

vim.lsp.enable('ruff')
