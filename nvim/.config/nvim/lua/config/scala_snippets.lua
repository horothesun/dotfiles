local M = {}

local initialized = false

function M.setup()
  if initialized then return end
  initialized = true

  local ls    = require("luasnip")
  local s     = ls.snippet
  local i     = ls.insert_node
  local f     = ls.function_node
  local fmt   = require("luasnip.extras.fmt").fmt

  -- get the current buffer filename without extension
  local function get_test_classname()
    local fullpath = vim.api.nvim_buf_get_name(0)
    local filename = fullpath:match("([^/\\]+)%.scala$")
    return filename or "Unknown"
  end

  local function get_base_classname()
    local name = get_test_classname()
    return name:gsub("Suite$", ""):gsub("Spec$", "")
  end

  local function is_test_file()
    return vim.fn.expand("%:p"):find("/test/") ~= nil
  end

  ls.add_snippets("scala", {
    s(
      { trig = "for-comprehension", name = "Scala for-comprehension" },
      fmt(
        [[
          for {{
            {} <- {}
          }} yield ()
        ]],
        {
          i(1, "_"),
          i(2, "..."),
        }
      ),
      {}
    ),
    s(
      { trig = "for-entry", name = "Scala for-comprehension entry" },
      fmt(
        [[
          {} <- {}
        ]],
        {
          i(1, "_"),
          i(2, "..."),
        }
      ),
      {}
    ),
    s(
      { trig = "munit-suite-scala3", name = "munit Scala 3 test suite" },
      fmt(
        [[
          import {}.*
          import {}.*
          import munit.ScalaCheckSuite
          import org.scalacheck.Gen
          import org.scalacheck.Prop.*

          class {} extends ScalaCheckSuite:

            test("{}"):
              assertEquals({}, {})

          object {}:

            val fortyTwo: Int = 42
        ]],
        {
          f(get_base_classname),
          f(get_test_classname),
          f(get_test_classname),
          i(1, "description"),
          i(2, "obtained"),
          i(3, "expected"),
          f(get_test_classname),
        }
      ),
      { show_condition = is_test_file, }
    ),
    s(
      { trig = "test-case-scala3", name = "Scala 3 test case" },
      fmt(
        [[
            test("{}"):
              assertEquals({}, {})
        ]],
        {
          i(1, "description"),
          i(2, "obtained"),
          i(3, "expected"),
        }
      ),
      { show_condition = is_test_file, }
    ),
    s(
      { trig = "prop-scala3", name = "Scala 3 PBT case" },
      fmt(
        [[
            property("{}"):
              forAll {{ (x: Int, y: Int) =>
                assert(Math.abs(x) + Math.abs(y.toLong) >= Math.abs(x))
              }}
        ]],
        {
          i(1, "description"),
        }
      ),
      { show_condition = is_test_file, }
    ),
    s(
      { trig = "munit-suite-scala2", name = "munit Scala 2 test suite" },
      fmt(
        [[
          import {}._
          import {}._
          import munit.ScalaCheckSuite
          import org.scalacheck.Gen
          import org.scalacheck.Prop._

          class {} extends ScalaCheckSuite {{

            test("{}") {{
              assertEquals({}, {})
            }}

          }}

          object {} {{

            val fortyTwo: Int = 42

          }}
        ]],
        {
          f(get_base_classname),
          f(get_test_classname),
          f(get_test_classname),
          i(1, "description"),
          i(2, "obtained"),
          i(3, "expected"),
          f(get_test_classname),
        }
      ),
      { show_condition = is_test_file, }
    ),
    s(
      { trig = "test-case-scala2", name = "Scala 2 test case" },
      fmt(
        [[
            test("{}") {{
              assertEquals({}, {})
            }}
        ]],
        {
          i(1, "description"),
          i(2, "obtained"),
          i(3, "expected"),
        }
      ),
      { show_condition = is_test_file, }
    ),
    s(
      { trig = "prop-scala2", name = "Scala 2 PBT case" },
      fmt(
        [[
            property("{}") {{
              forAll {{ (x: Int, y: Int) =>
                assert(Math.abs(x) + Math.abs(y.toLong) >= Math.abs(x))
              }}
            }}
        ]],
        {
          i(1, "description"),
        }
      ),
      { show_condition = is_test_file, }
    ),
  })
end

return M
