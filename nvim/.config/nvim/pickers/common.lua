local function picker(opts, prompt_title, items, selection_mapper)
  opts = opts or {}
  local pickers = require('telescope.pickers')
  local finders = require('telescope.finders')
  local configuration = require('telescope.config').values
  return pickers.new(opts, {
    prompt_title = prompt_title,
    finder = finders.new_table { results = items },
    sorter = configuration.generic_sorter(opts),
    attach_mappings = function(prompt_bufnr, _)
      local actions = require('telescope.actions')
      actions.select_default:replace(
        function()
          actions.close(prompt_bufnr)
          local action_state = require('telescope.actions.state')
          local selection = action_state.get_selected_entry()
          local output = selection_mapper(selection[1])
          vim.api.nvim_put(
            { output }, -- lines
            '',          -- type: edit behavior
            true,       -- after: insert after cursor
            true       -- follow: place cursor at end of inserted text
          )
        end
      )
      return true
    end,
  })
end

local function split(str, delimiter_pattern)
  local result = {}
  for part in str:gmatch(delimiter_pattern) do
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

local function first_word(str)
  return words(str)[1]
end

local function unquoted_first_word(str)
  return first_word(str):sub(2, -2)
end


local common = {}

common.picker = picker
common.lines = lines
common.first_word = first_word
common.unquoted_first_word = unquoted_first_word

return common
