-- ~/.config/nvim/lua/config/haskell_snippets.lua
local M = {}

local initialized = false

function M.setup()
  if initialized then
    return
  end
  initialized = true

  local ls = require("luasnip")
  local s  = ls.snippet
  local t  = ls.text_node
  local i  = ls.insert_node
  local f  = ls.function_node

  local function module_name()
    local name = vim.fn.expand("%:r")
    name = name:gsub("^.*/src/", "")
    name = name:gsub("^.*/test/", "")
    name = name:gsub("^src/", "")
    name = name:gsub("^test/", "")
    name = name:gsub("/", ".")
    return name
  end

  local function is_source_file()
    local path = vim.fn.expand("%:p")
    -- "src" might appear in the absolute path before the project root folder
    return path:match("/src/") ~= nil and path:match("/test/") == nil
  end

  local function is_test_file()
    return vim.fn.expand("%:p"):match("/test/") ~= nil
  end

  ------------------------------------------------------------
  -- 1. Module header for src/ files
  ------------------------------------------------------------
  ls.add_snippets("haskell", {
    s(
      { trig = "module-src", name = "Haskell module header (src)" },
      {
        t("module "),
        f(module_name, {}),
        t({ " where", "", "" }),
        i(0),
      },
      {
        show_condition = is_source_file,
      }
    ),
  })

  ------------------------------------------------------------
  -- 2. Test module header (imports + spec skeleton)
  ------------------------------------------------------------
  ls.add_snippets("haskell", {
    s(
      { trig = "module-test", name = "Hspec test module (test)" },
      {
        t("module "),
        f(module_name, {}),
        t({ " where", "", "" }),
        t("import Test.Hspec"),
        t({ "", "import Test.Hspec.QuickCheck", "import Test.QuickCheck", "", "" }),

        t("spec :: Spec"),
        t({ "", "spec = do", "  " }),
        i(0),
      },
      {
        show_condition = is_test_file,
      }
    ),
  })

  ------------------------------------------------------------
  -- 3. Snippet: it-block
  ------------------------------------------------------------
  ls.add_snippets("haskell", {
    s(
      { trig = "it", name = "Hspec it block" },
      {
        t("it \""), i(1, "description"), t("\" $"),
        t({ "", "  " }), i(2, "actual"),
        t(" `shouldBe` "), i(3, "expected"),
      },
      {
        show_condition = is_test_file,
      }
    ),
  })

  ------------------------------------------------------------
  -- 4. Snippet: QuickCheck prop-block
  ------------------------------------------------------------
  ls.add_snippets("haskell", {
    s(
      { trig = "prop", name = "Hspec it block" },
      {
        t("prop \""), i(1, "description"), t("\" $"),
        t({ "", "  " }), i(2, "actual"),
        t(" `shouldBe` "), i(3, "expected"),
      },
      {
        show_condition = is_test_file,
      }
    ),
  })
end

return M
