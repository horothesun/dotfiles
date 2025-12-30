-- https://github.com/neovim/nvim-lspconfig/blob/master/lsp/bashls.lua

local M = {}

function M.setup()

  local lsp = require("config.lsp")

  -- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
  lsp.setup()

  -- TODO: implement!!! 🔥🔥🔥
  -- require("config.bash_snippets").setup()

  vim.lsp.config['bashls'] = {
    cmd = { 'bash-language-server', 'start' },
    settings = {
      bashIde = {
        -- Glob pattern for finding and parsing shell script files in the workspace.
        -- Used by the background analysis features across files.

        -- Prevent recursive scanning which will cause issues when opening a file
        -- directly in the home directory (e.g. ~/foo.sh).
        --
        -- Default upstream pattern is "**/*@(.sh|.inc|.bash|.command)".
        globPattern = vim.env.GLOB_PATTERN or '*@(.sh|.inc|.bash|.command)',
      },
    },
    filetypes = { 'bash', 'sh' },
    root_markers = { '.git' },
  }

  vim.lsp.enable('bashls')
end

return M
