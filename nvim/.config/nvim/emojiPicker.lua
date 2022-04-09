local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

local function split(str, delimiterPattern)
  local result = {}
  for part in str:gmatch(delimiterPattern) do
    table.insert(result, part)
  end
  return result
end

local function lines(str)
  return split(str, '[^\n]+')
end

local function words(str)
  return split(str, '%S+')
end

-- optimisation: only return 1st word of the split
local function extractFirstWord(str)
  return words(str)[1]
end

-- our picker function
local emojiPicker = function(opts)
  local uniOutput = vim.fn.system("uni e all")

  local multilineUniOutput = lines(uniOutput)
  table.remove(multilineUniOutput, 1) -- remove 'name (cldr)' header

  opts = opts or {}
  pickers.new(opts, {
    prompt_title = "Emojis",
    finder = finders.new_table {
      results = multilineUniOutput
    },
    sorter = conf.generic_sorter(opts),
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(
        function()
          actions.close(prompt_bufnr)
          local selection = action_state.get_selected_entry()
          local emoji = extractFirstWord(selection[1])
          vim.api.nvim_put({ emoji }, "", false, true)
        end
      )
      return true
    end,
  }):find()
end

-- launch the picker
emojiPicker()
-- emojiPicker(require("telescope.themes").get_dropdown{})
