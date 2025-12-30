local ht  = require("haskell-tools")
local lsp = require("config.lsp")

-- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
lsp.setup()

require("config.haskell_snippets").setup()

local bufnr = vim.api.nvim_get_current_buf()
local opts = { noremap = true, silent = true, buffer = bufnr }

-- Hoogle signature lookup
vim.keymap.set("n", "<leader>ho", ht.hoogle.hoogle_signature, opts)

-- Evaluate all code snippets
vim.keymap.set("n", "<leader>ea", ht.lsp.buf_eval_all, opts)
