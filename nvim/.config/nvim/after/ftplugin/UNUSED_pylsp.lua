-- ~/.config/nvim/after/ftplugin/python.lua

local lsp = require("config.lsp")

lsp.setup()

-- TODO: implement!!! 🔥🔥🔥
-- require("config.python_snippets").setup()

vim.lsp.config['pylsp'] = {
  cmd = { 'pylsp' },
  filetypes = { 'python' },
  root_markers = {
    'pyproject.toml',
    'setup.py',
    'setup.cfg',
    'requirements.txt',
    'Pipfile',
    '.git',
  },
}

vim.lsp.enable('pylsp')
