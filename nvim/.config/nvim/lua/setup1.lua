-- appearance of popup menu for autocomplete
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Diagnostic
local diag_opts = { noremap = true, silent = true }

-- Diagnostic to QuickFix/LocalFix lists
vim.keymap.set("n", "<leader>dq", function()
  vim.diagnostic.setqflist { open = false }
  vim.cmd("cfirst")
end, diag_opts)
vim.keymap.set("n", "<leader>dl", function()
  vim.diagnostic.setloclist { open = false }
  vim.cmd("lfirst")
end, diag_opts)


vim.diagnostic.config {
  underline = true,
  signs = true,
  virtual_text = false,
  virtual_lines = { current_line = true },
  float = {
    show_header = true,
    source = "if_many",
    border = "rounded",
  },
  update_in_insert = false,
  severity_sort = false,
}

-- same as Ctrl-W d , but with autofocus on the floating box
vim.keymap.set("n", "gl", function()
  local _, winid = vim.diagnostic.open_float(nil, {})
  if winid and vim.api.nvim_win_is_valid(winid) then
    vim.api.nvim_set_current_win(winid)
  end
end, { silent = true, desc = "Diagnostics float (enter)" })

-- Other plugins that we wanna load for every projects
require("spaceless").setup()
require("plugin.projectionist")
require("plugin.gitsigns")
require("plugin.telescope")
