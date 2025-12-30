-- appearance of popup menu for autocomplete
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Diagnostic
local diag_opts = { noremap = true, silent = true }

vim.keymap.set("n", "[e", function()
  vim.diagnostic.jump({ count = -1, float = true })
end, diag_opts)

vim.keymap.set("n", "]e", function()
  vim.diagnostic.jump({ count = 1, float = true })
end, diag_opts)

vim.keymap.set("n", "<leader>dl", vim.diagnostic.setloclist, diag_opts)

vim.diagnostic.config {
  underline = true,
  signs = true,
  virtual_text = false,
  float = {
    show_header = true,
    source = "if_many",
    border = "rounded",
    focusable = true,
  },
  update_in_insert = false,
  severity_sort = false,
}

-- same as Ctrl-W d , but with autofocus on the floating box
vim.keymap.set("n", "gl", function()
  local _, winid = vim.diagnostic.open_float(nil, {
    focusable = true,
    border = "rounded",
  })
  if winid and vim.api.nvim_win_is_valid(winid) then
    vim.api.nvim_set_current_win(winid)
  end
end, { silent = true, desc = "Diagnostics float (enter)" })

-- Other plugins that we wanna load for every projects
require("spaceless").setup()
require("config.projectionist")
require("plugin/gitsigns")
require("plugin/telescope")
