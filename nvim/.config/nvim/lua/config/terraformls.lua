-- https://github.com/neovim/nvim-lspconfig/blob/master/lsp/terraformls.lua

local M = {}

function M.setup()
  local lsp = require("config.lsp")

  -- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
  lsp.setup()

  -- TODO: implement!!! 🔥🔥🔥
  -- require("config.terraform_snippets").setup()

  vim.lsp.config["terraformls"] = {
    cmd = { "terraform-ls", "serve" },
    filetypes = { "hcl", "terraform", "tf" },
    root_markers = { ".terraform", ".git" },
  }

  vim.lsp.enable("terraformls")
end

return M
