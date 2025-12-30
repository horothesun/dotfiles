-- https://github.com/neovim/nvim-lspconfig/blob/master/lsp/ruff.lua

local M = {}

function M.setup()
  local lsp = require("config.lsp")

  -- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
  lsp.setup()

  -- TODO: implement!!! 🔥🔥🔥
  -- require("config.python_snippets").setup()

  vim.lsp.config["ruff"] = {
    cmd = { "ruff", "server" },
    filetypes = { "python" },
    root_markers = {
      "pyproject.toml",
      "ruff.toml",
      ".ruff.toml",
      ".git"
    },
    settings = {},
  }

  vim.lsp.enable("ruff")
end

return M
