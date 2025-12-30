local telescope = require('telescope')
telescope.setup {
  defaults = {
    file_sorter = require('telescope.sorters').get_fzy_sorter,
    prompt_prefix = '> ',
    color_devicons = true,
    file_previewer = require('telescope.previewers').vim_buffer_cat.new,
    grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
    qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
    -- vimgrep_arguments = {
    --   'rg',
    --   '--color=never',
    --   '--no-heading',
    --   '--with-filename',
    --   '--line-number',
    --   '--column',
    --   '--smart-case',
    --   '--unrestricted', -- disable .gitignore handling
    --   '--unrestricted'  -- search hidden files and directories
    -- },
    layout_config = { width = 0.9, height = 0.9 },
    mappings = {
      i = {
        ['<esc>'] = require('telescope.actions').close,
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
      additional_args = function(opts)
        return { "--hidden" }
      end
    }
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true
    }
  }
}

telescope.load_extension('fzy_native')

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files,       { desc = 'Telescope find files' })
vim.keymap.set('n', '<C-p>',      builtin.live_grep,        { desc = 'Telescope live grep' })
vim.keymap.set('n', '<leader>fb', ":Telescope buffers<CR>", { desc = 'Telescope buffers' })
vim.keymap.set('n', '<leader>fh', builtin.help_tags,        { desc = 'Telescope help tags' })
vim.keymap.set('n', '<leader>fq', builtin.quickfix,         { desc = 'Telescope quickfix' })

vim.keymap.set('n', '<leader>fe', ":luafile ~/.config/nvim/pickers/emoji_picker.lua<CR>",  { desc = "Telescope emoji picker"})
vim.keymap.set('n', '<leader>fs', ":luafile ~/.config/nvim/pickers/symbol_picker.lua<CR>", { desc = "Telescope symbol picker"})
