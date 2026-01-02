-- https://github.com/neovim/nvim-lspconfig/blob/master/lsp/basedpyright.lua

local M = {}

function M.setup()
  local lsp = require("config.lsp")

  -- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
  lsp.setup()

  -- TODO: implement!!! 🔥🔥🔥
  -- require("config.python_snippets").setup()

  local function set_python_path(command)
    local path = command.args
    local clients = vim.lsp.get_clients {
      bufnr = vim.api.nvim_get_current_buf(),
      name = "basedpyright",
    }
    for _, client in ipairs(clients) do
      if client.settings then
        ---@diagnostic disable-next-line: param-type-mismatch
        client.settings.python = vim.tbl_deep_extend("force", client.settings.python or {}, { pythonPath = path })
      else
        client.config.settings = vim.tbl_deep_extend("force", client.config.settings, { python = { pythonPath = path } })
      end
      client:notify("workspace/didChangeConfiguration", { settings = nil })
    end
  end

  vim.lsp.config["basedpyright"] = {
    cmd = { "basedpyright-langserver", "--stdio" },
    filetypes = { "python" },
    root_markers = {
      "pyrightconfig.json",
      "pyproject.toml",
      "setup.py",
      "setup.cfg",
      "requirements.txt",
      "Pipfile",
      ".git",
    },
    settings = {
      basedpyright = {
        analysis = {
          autoSearchPaths = true,
          useLibraryCodeForTypes = true,
          diagnosticMode = "openFilesOnly",
        },
      },
    },
    on_attach = function(client, bufnr)
      vim.api.nvim_buf_create_user_command(bufnr, "LspPyrightOrganizeImports", function()
        local params = {
          command = "basedpyright.organizeimports",
          arguments = { vim.uri_from_bufnr(bufnr) },
        }

        -- Using client:request() directly because "basedpyright.organizeimports" is private
        -- (not advertised via capabilities), which client:exec_cmd() refuses to call.
        -- https://github.com/neovim/neovim/blob/c333d64663d3b6e0dd9aa440e433d346af4a3d81/runtime/lua/vim/lsp/client.lua#L1024-L1030
        ---@diagnostic disable-next-line: param-type-mismatch
        client:request("workspace/executeCommand", params, nil, bufnr)
      end, {
        desc = "Organize Imports",
      })

      vim.api.nvim_buf_create_user_command(bufnr, "LspPyrightSetPythonPath", set_python_path, {
        desc = "Reconfigure basedpyright with the provided python path",
        nargs = 1,
        complete = "file",
      })

      -- Disable all capabilities except hoverProvider
      client.server_capabilities.completionProvider = {}
      client.server_capabilities.definitionProvider = false
      client.server_capabilities.typeDefinitionProvider = false
      client.server_capabilities.implementationProvider = false
      client.server_capabilities.referencesProvider = false
      client.server_capabilities.documentSymbolProvider = false
      client.server_capabilities.workspaceSymbolProvider = false
      client.server_capabilities.codeActionProvider = false
      client.server_capabilities.documentFormattingProvider = false
      client.server_capabilities.documentRangeFormattingProvider = false
      client.server_capabilities.renameProvider = false
      client.server_capabilities.signatureHelpProvider = {}
      client.server_capabilities.documentHighlightProvider = false
      client.server_capabilities.foldingRangeProvider = false
      client.server_capabilities.semanticTokensProvider = {
        legend = {
          tokenTypes = {},
          tokenModifiers = {},
        }
      }
      client.server_capabilities.declarationProvider = false
      client.server_capabilities.callHierarchyProvider = false
      client.server_capabilities.diagnosticProvider = {
        interFileDependencies = false,
        workspaceDiagnostics = false,
      }

      -- Enable hoverProvider
      client.server_capabilities.hoverProvider = true
    end,
  }

  vim.lsp.enable("basedpyright")
end

return M
