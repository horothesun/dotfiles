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
    on_attach = function(client, _)
      if client.name == "ruff" then
        -- disable hover in favor of basedpyright
        client.server_capabilities.hoverProvider = false
      end
    end
  }

  vim.lsp.enable("ruff")
end

return M
