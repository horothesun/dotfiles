-- appearance of popup menu for autocomplete
vim.opt.completeopt = { "menu", "menuone", "noselect" }

-- Diagnostic

-- Diagnostic to QuickFix/LocalFix lists
vim.keymap.set("n", "<leader>dq", function()
  vim.diagnostic.setqflist { open = false }
  vim.cmd("cfirst")
end, { silent = true, desc = "Diagnostics: workspace (quickfix)" })
vim.keymap.set("n", "<leader>dl", function()
  vim.diagnostic.setloclist { open = false }
  vim.cmd("lfirst")
end, { silent = true, desc = "Diagnostics: buffer (loclist)" })

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
vim.keymap.set("n", "<leader>df", function()
  local _, winid = vim.diagnostic.open_float(nil, {})
  if winid and vim.api.nvim_win_is_valid(winid) then
    vim.api.nvim_set_current_win(winid)
  end
end, { silent = true, desc = "Diagnostics float (enter)" })

-- Highlight on yank for visual feedback
local group = vim.api.nvim_create_augroup("UserYankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  group = group,
  callback = function() vim.highlight.on_yank({ timeout = 200 }) end,
  desc = "highlight yanked text"
})

-- Other plugins that we wanna load for every projects
require("plugin.projectionist")
require("plugin.gitsigns")
require("plugin.telescope")
require("nvim-autopairs").setup {}
