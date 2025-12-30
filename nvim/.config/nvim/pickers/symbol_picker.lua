local function current_path()
  return debug.getinfo(2, "S").source:sub(2):match("(.*/)")
end

package.path = package.path .. ";" .. current_path() .. "common.lua;"

local symbol_picker = function(opts)
  local command = "uni search all"
  local uni_output = vim.fn.system(command)
  local common = require("common")
  local uni_output_lines = common.lines(uni_output)
  table.remove(uni_output_lines, 1) -- remove header
  return common.picker(opts, "Symbols", uni_output_lines, common.unquoted_first_word)
end

-- launch the picker
symbol_picker():find()
-- symbol_picker(require("telescope.themes").get_dropdown{}):find()
