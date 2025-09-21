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

vim.keymap.set('n', '<C-p>', ":Telescope live_grep<CR>", buf_opts)
vim.keymap.set('n', '<leader>ff', ":Telescope find_files<CR>", buf_opts)
vim.keymap.set('n', '<leader>fq', ":Telescope quickfix<CR>", buf_opts)
vim.keymap.set('n', '<leader>fb', ":Telescope buffers<CR>", buf_opts)
vim.keymap.set('n', '<leader>fh', ":Telescope help_tags<CR>", buf_opts)
vim.keymap.set('n', '<leader>fe', ":luafile ~/.config/nvim/pickers/emoji_picker.lua<CR>", buf_opts)
vim.keymap.set('n', '<leader>fs', ":luafile ~/.config/nvim/pickers/symbol_picker.lua<CR>", buf_opts)
