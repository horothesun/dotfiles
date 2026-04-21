local M = {}

local function gh(repo)
  return "https://github.com/" .. repo
end

local function runPostInstallationHook(cmd, opts)
  opts = opts or {}
  local cwd = opts.cwd

  local ok, err = pcall(vim.system, cmd, { cwd = cwd, text = true }, function(res)
    vim.schedule(function()
      local cmd_str = table.concat(cmd, " ")
      local message, level
      if res.code == 0 then
        message = ("✅ Post installation hook %s succeeded"):format(cmd_str)
        level = vim.log.levels.INFO
      else
        message = string.format(
          "❌ Post installation hook %s failed\ncwd: %s\nexit: %s\n\nstdout:\n%s\n\nstderr:\n%s",
          cmd_str,
          cwd or "(nil)",
          tostring(res.code),
          res.stdout or "",
          res.stderr or ""
        )
        level = vim.log.levels.ERROR
      end

      vim.notify(message, level, { title = "vim.pack" })
    end)
  end)

  if not ok then
    local cmd_str = table.concat(cmd, " ")
    vim.notify(
      ("Post installation hook %s failed to start: %s"):format(cmd_str, tostring(err)),
      vim.log.levels.ERROR,
      { title = "vim.pack" }
    )
  end
end

local function postProcessingAfterInstallation(ev)
  if not ev.data or not ev.data.kind or not ev.data.spec or not ev.data.spec.name then
    return
  end
  local kind = ev.data.kind
  local name = ev.data.spec.name
  local path = ev.data.path
  if kind == "install" or kind == "update" then
    if name == "cornelis" then
      runPostInstallationHook({ "stack", "build" }, { cwd = path })
    elseif name == "telescope-fzf-native" then
      runPostInstallationHook({ "make" }, { cwd = path })
    elseif name == "markdown-preview" then
      runPostInstallationHook({ "cd app && yarn install" }, { cwd = path })
    end
  end
end

function M.setup()
  local packchanged_group = vim.api.nvim_create_augroup("PackChangedPostInstall", { clear = true })
  vim.api.nvim_create_autocmd("PackChanged", {
    group = packchanged_group,
    callback = postProcessingAfterInstallation,
  })

  -- Global plugins
  vim.pack.add({
    gh("lewis6991/gitsigns.nvim"),
    gh("axelf4/vim-strip-trailing-whitespace"),
    gh("windwp/nvim-autopairs"), -- Auto-pair brackets/quotes

    gh("nvim-lua/plenary.nvim"),
    gh("nvim-telescope/telescope.nvim"),
    { src = gh("nvim-telescope/telescope-fzf-native.nvim"), name = "telescope-fzf-native" },
    gh("nvim-telescope/telescope-symbols.nvim"),

    gh("hrsh7th/nvim-cmp"),         -- Core completion framework
    gh("hrsh7th/cmp-nvim-lsp"),     -- LSP completion source
    gh("L3MON4D3/LuaSnip"),         -- Lua-based snippet engine
    gh("saadparwaiz1/cmp_luasnip"), -- LuaSnip completion source

    gh("tpope/vim-projectionist"),  -- Go-to-tests

    gh("j-hui/fidget.nvim"),        -- LSP loader indicator

    -- Colour schemes
    -- gh("gruvbox-community/gruvbox"), -- alternative
    gh("sainnhe/gruvbox-material"),
  }, { load = true })

  -- Optional plugins (they are loaded on specific buffers in ftplugin)
  vim.pack.add({
    gh("scalameta/nvim-metals"),
    { src = gh("Mrcjkb/haskell-tools.nvim"), ft = { "haskell" } },
    -- { src = gh("iamcco/markdown-preview.nvim"), ft = { "markdown" }, name = "markdown-preview" },
    -- { src = gh("kana/vim-textobj-user"),     ft = { "agda" } },
    -- { src = gh("neovimhaskell/nvim-hs.vim"), ft = { "agda" } },
    -- { src = gh("agda/cornelis"),             ft = { "agda" } },
  }, { load = false })
end

return M
