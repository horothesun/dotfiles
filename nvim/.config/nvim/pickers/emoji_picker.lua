local function current_path()
   return debug.getinfo(2, "S").source:sub(2):match("(.*/)")
end

package.path = package.path .. ';' .. current_path() .. 'common.lua;'

local emoji_picker = function(opts)
  local command = 'uni emoji all'
  local uni_output = vim.fn.system(command)
  local common = require('common')
  local uni_output_lines = common.lines(uni_output)
  table.remove(uni_output_lines, 1) -- remove 'name (cldr)' header
  return common.picker(opts, 'Emojis', uni_output_lines, common.first_word)
end

-- launch the picker
emoji_picker():find()
-- emoji_picker(require('telescope.themes').get_dropdown{}):find()
