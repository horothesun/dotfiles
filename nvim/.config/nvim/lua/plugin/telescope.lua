local telescope = require("telescope")
telescope.setup {
  defaults = {
    prompt_prefix = "> ",
    color_devicons = true,
    layout_config = { width = 0.9, height = 0.9 },
    mappings = {
      i = {
        ["<esc>"] = require("telescope.actions").close,
      }
    },
    file_ignore_patterns = {
      ".git/",
      ".idea/",
      ".metals/",
      ".bloop/",
      "node_modules/"
    }
  },
  pickers = {
    find_files = { hidden = true },
    live_grep = {
      additional_args = function(_)
        return { "--hidden" }
      end
    }
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case", -- or "ignore_case" or "respect_case" (default case_mode is "smart_case")
    }
  }
}

telescope.load_extension("fzf")

local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "Telescope find files" })
vim.keymap.set("n", "<C-p>", builtin.live_grep, { desc = "Telescope live grep" })
vim.keymap.set("n", "<leader>fb", ":Telescope buffers<CR>", { desc = "Telescope buffers" })
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Telescope help tags" })
vim.keymap.set("n", "<leader>fq", builtin.quickfix, { desc = "Telescope quickfix" })

vim.keymap.set("n", "<leader>fe", function() builtin.symbols({ sources = { "emoji" } }) end,
  { desc = "Telescope emoji picker" })
vim.keymap.set("n", "<leader>fn", function() builtin.symbols({ sources = { "nerd" } }) end,
  { desc = "Telescope nerd symbol picker" })
vim.keymap.set("n", "<leader>fs", function() builtin.symbols({ sources = { "math" } }) end,
  { desc = "Telescope math symbol picker" })
