-- ~/.config/nvim/lua/config/projectionist.lua
local M = {}

vim.g.projectionist_heuristics = {
  ["stack.yaml"] = {
    -- source -> test
    ["src/*.hs"] = {
      alternate = "test/{}Spec.hs",
      type = "source",
    },
    -- test -> source
    ["test/*Spec.hs"] = {
      alternate = "src/{}.hs",
      type = "test",
    },
  },

  ["build.sbt"] = {
    -- source -> test
    ["src/main/scala/*.scala"] = {
      alternate = {
        "src/test/scala/{}Suite.scala",
        "src/test/scala/{}Spec.scala",
      },
      type = "source",
    },
    -- *Suite test -> source
    ["src/test/scala/*Suite.scala"] = {
      alternate = "src/main/scala/{}.scala",
      type = "test",
    },
    -- *Spec test -> source
    ["src/test/scala/*Spec.scala"] = {
      alternate = "src/main/scala/{}.scala",
      type = "test",
    },
  },
}

-- Keymap: <leader>gt = "go to test" (and back a source)
vim.keymap.set("n", "<leader>gt", "<Cmd>A<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>gT", "<Cmd>AV<CR>", { noremap = true, silent = true })

return M

