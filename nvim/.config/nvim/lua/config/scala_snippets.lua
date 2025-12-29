-- ~/.config/nvim/lua/config/scala_snippets.lua
local M = {}

local initialized = false

function M.setup()
  if initialized then return end
  initialized = true

  local ls = require("luasnip")
  local s  = ls.snippet
  local t  = ls.text_node
  local i  = ls.insert_node
  local f  = ls.function_node

  -- get the current buffer filename without extension
  local function get_filename()
    local fullpath = vim.api.nvim_buf_get_name(0)
    local filename = fullpath:match("([^/\\]+)%.scala$")
    return filename or "Unknown"
  end

  -- base class = strip Suite/Spec
  local function base_classname_fn()
    local name = get_filename()
    return name:gsub("(Suite|Spec)$", "")
  end

  -- test class = full filename
  local function test_classname_fn()
    return get_filename()
  end

  ls.add_snippets("scala", {
    s(
      { trig = "munit-test", name = "munit Scala 3 test" },
      {
        f(function() return "import " .. base_classname_fn() .. ".*" end),
        f(function() return "import " .. test_classname_fn() .. ".*" end),
        t({
          "import munit.ScalaCheckSuite",
          "import org.scalacheck.Gen",
          "import org.scalacheck.Prop.*",
          "",
        }),
        f(function() return "class " .. test_classname_fn() .. " extends ScalaCheckSuite:" end),
        t({ "", "  test(\"" }),
        i(1, "fortytwo == 42"),
        t("\"):"),
        t({ "", "    " }),
        i(2, "assertEquals(fortyTwo, 42)"),
        t({ "", "", }),
        f(function() return "object " .. test_classname_fn() .. ":" end),
        t({ "", "  val fortyTwo: Int = 42" }),
      }
    )
  })

end

return M

