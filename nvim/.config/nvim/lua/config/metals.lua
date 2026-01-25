-- https://github.com/neovim/nvim-lspconfig/blob/master/lsp/metals.lua

local M = {}

function M.setup()
  local metals = require("metals")
  local lsp    = require("config.lsp")

  -- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
  lsp.setup()

  require("config.scala_snippets").setup()

  local cfg = metals.bare_config()

  cfg.capabilities = lsp.capabilities

  cfg.settings = {
    showImplicitArguments = true,
    serverVersion = "2.0.0-M7", -- "latest.snapshot"
    excludedPackages = {
      "akka.actor.typed.javadsl",
      "com.github.swagger.akka.javadsl",
    },
  }

  vim.api.nvim_create_autocmd("FileType", {
    pattern = { "scala", "sbt" },
    callback = function()
      metals.initialize_or_attach(cfg)
    end,
  })

  -- if setup() runs after a scala buffer is already open
  if vim.bo.filetype == "scala" or vim.bo.filetype == "sbt" then
    metals.initialize_or_attach(cfg)
  end

  -- Check if a file exists in the root directory
  -- local function has_scalafmt()
  --   local root_files = vim.lsp.buf.list_workspace_folders()
  --   for _, folder in ipairs(root_files) do
  --     local path = folder .. "/.scalafmt.conf"
  --     if vim.loop.fs_stat(path) then
  --       return true
  --     end
  --   end
  --   return false
  -- end

  -- Autoformat Scala files on save if .scalafmt.conf exists
  -- vim.api.nvim_create_autocmd("BufWritePre", {
  --   pattern = "*.scala",
  --   callback = function()
  --     if has_scalafmt() then
  --       local clients = vim.lsp.get_clients()
  --       for _, client in pairs(clients) do
  --         if client.name == "metals" and client.server_capabilities.documentFormattingProvider then
  --           vim.lsp.buf.format({ async = false })
  --           return
  --         end
  --       end
  --     end
  --   end
  -- })
end

return M
