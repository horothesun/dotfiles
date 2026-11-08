local ht  = require("haskell-tools")
local lsp = require("config.lsp")

-- Ensure shared LSP behaviour is initialized (on_attach, <leader>ls, etc.)
lsp.setup()

require("config.haskell_snippets").setup()

local bufnr = vim.api.nvim_get_current_buf()
local opts = { noremap = true, silent = true, buffer = bufnr }

-- Hoogle signature lookup
vim.keymap.set("n", "<leader>ho", ht.hoogle.hoogle_signature, opts)

-- Evaluate all code snippets
vim.keymap.set("n", "<leader>ea", ht.lsp.buf_eval_all, opts)



-- ---------------------------------------------------------------------------
-- Online documentation lookup
-- ---------------------------------------------------------------------------

local LOG_FILE = vim.fn.stdpath("state") .. "/haskell_open_docs.log"

local function timestamp()
  return os.date("%Y-%m-%d %H:%M:%S")
end

local function log(message)
  message = tostring(message)

  for line in (message .. "\n"):gmatch("(.-)\n") do
    vim.fn.writefile({ "[" .. timestamp() .. "] " .. line }, LOG_FILE, "a")
  end
end

local function log_separator(lookup_type)
  vim.fn.writefile(
    {
      "",
      "============================================================",
      "[" .. timestamp() .. "] New " .. lookup_type .. " documentation lookup",
    },
    LOG_FILE,
    "a"
  )
end

local function notify(message, level)
  vim.notify(message, level or vim.log.levels.INFO)
end

local function trim(value)
  return (value:gsub("^%s+", ""):gsub("%s+$", ""))
end

local function hover_contents_to_string(contents)
  if type(contents) == "string" then
    return contents
  end

  if type(contents) == "table" then
    -- MarkupContent: { kind = "markdown", value = "..." }
    if contents.value then
      return contents.value
    end

    -- MarkedString array or a single MarkedString object.
    local parts = {}

    for _, item in ipairs(contents) do
      if type(item) == "string" then
        table.insert(parts, item)
      elseif type(item) == "table" then
        if item.value then
          table.insert(parts, item.value)
        elseif item.language and item.value then
          table.insert(parts, item.value)
        end
      end
    end

    return table.concat(parts, "\n")
  end

  return ""
end

-- Determine the Haddock anchor type for a symbol.
local function get_haddock_anchor(entity_name, hover_text)
  if not entity_name or entity_name == "" then
    return nil
  end

  local plain_text = (hover_text or ""):gsub("[%*`_]", ""):gsub("\r", "")

  -- Inspect the first Haskell signature/declaration.
  local declaration = plain_text:match("^%s*```[%w]*%s*\n(.-)\n```")

  if not declaration then
    declaration = plain_text
  end

  -- Type and typeclass declarations.
  if declaration:match("%f[%a]data%s+")
      or declaration:match("%f[%a]newtype%s+")
      or declaration:match("%f[%a]type%s+")
      or declaration:match("%f[%a]class%s+") then
    return "t"
  end

  -- Functions, values, and class methods.
  return "v"
end

-- Get the module and package information from the HLS hover response.
--
-- Examples:
--
--   Defined in ‘Data.Aeson.Types.ToJSON’ (aeson-2.2.5.1)
--     -> module: Data.Aeson.Types
--     -> entity: ToJSON
--
--   Defined in ‘GHC.Internal.Base’ (ghc-internal-9.1401.0)
--     -> module: GHC.Internal.Base
--     -> entity: Semigroup
--
-- The final segment is only split off when it matches the symbol
-- extracted from the hover signature.
local function get_module_from_hover()
  local clients = vim.lsp.get_clients({ bufnr = bufnr })

  local hover_client = nil

  for _, client in ipairs(clients) do
    if client.server_capabilities and client.server_capabilities.hoverProvider then
      hover_client = client
      break
    end
  end

  if not hover_client then
    log("No LSP client with hover support found")
    return nil
  end

  local params = vim.lsp.util.make_position_params(0, hover_client.offset_encoding)
  local response = hover_client:request_sync("textDocument/hover", params, 10000, bufnr)

  if not response then
    log("Hover request timed out or returned no response")
    return nil
  end

  if response.err then
    log("Hover request error: " .. vim.inspect(response.err))
    return nil
  end

  if not response.result or not response.result.contents then
    log("Hover response contained no contents")
    return nil
  end

  local hover_text = hover_contents_to_string(response.result.contents)

  log("Raw hover response: " .. vim.inspect(hover_text))

  -- Remove Markdown formatting while preserving the actual text.
  local plain_text = hover_text
      :gsub("[%*`_]", "")
      :gsub("\r", "")

  -- Extract the symbol name from the signature, e.g.:
  --   Semigroup :: Type -> Constraint
  local symbol_name = plain_text:match("[\n\r]%s*([%w_']+)%s*::") or plain_text:match("^%s*([%w_']+)%s*::")

  if not symbol_name then
    log("Could not extract symbol name from hover signature")
    return nil
  end

  -- Find the "Defined in" section.
  local defined_pos = plain_text:find("Defined in ", 1, true)

  if not defined_pos then
    log("Could not find 'Defined in' in hover response")
    return nil
  end

  local definition = plain_text:sub(defined_pos + #"Defined in ")

  -- Parse the module or qualified symbol between quotation marks.
  -- HLS normally uses curly quotation marks, but support straight
  -- quotation marks as well.
  local module_start, module_end = definition:find("‘", 1, true)

  local closing_quote = "’"

  if not module_start then
    module_start, module_end = definition:find("'", 1, true)
    closing_quote = "'"
  end

  if not module_start then
    log("Could not find opening quote in Defined in section")
    return nil
  end

  local closing_start = definition:find(closing_quote, module_end + 1, true)

  if not closing_start then
    log("Could not find closing quote in Defined in section")
    return nil
  end

  local defined_name = definition:sub(module_end + 1, closing_start - 1)

  -- Extract the package identifier from the parentheses.
  local package_open = definition:find("(", closing_start + #closing_quote, true)

  if not package_open then
    log("Could not find package identifier in hover response")
    return nil
  end

  local package_close = definition:find(")", package_open + 1, true)

  if not package_close then
    log("Could not find closing package parenthesis")
    return nil
  end

  local package_id = trim(definition:sub(package_open + 1, package_close - 1))

  if defined_name == "" or package_id == "" then
    log("Parsed module name or package identifier was empty")
    return nil
  end

  -- HLS may return either a fully qualified symbol or just a module.
  --
  -- Only split the final segment when it matches the symbol name.
  local actual_module = defined_name
  local entity_name = symbol_name

  local possible_module, possible_entity = defined_name:match("^(.+)%.([^.]+)$")

  if possible_entity and possible_entity == symbol_name then
    actual_module = possible_module
    entity_name = possible_entity
  end

  log("Hover module: " .. actual_module)
  log("Hover entity: " .. tostring(entity_name))
  log("Hover package: " .. package_id)

  return actual_module, package_id, entity_name, plain_text
end

-- Run a shell command and return its trimmed standard output.
local function run_command(command)
  log("Running command: " .. command)

  local output = vim.fn.system(command)
  local exit_code = vim.v.shell_error

  if exit_code ~= 0 then
    log("Command failed with exit code " .. exit_code)
    log("Command output: " .. output)
    return nil
  end

  return trim(output)
end

-- Resolve a package identifier from a module name when no package
-- identifier was supplied.
local function get_package_for_module(module_name)
  local escaped_module = vim.fn.shellescape(module_name)

  local output = run_command("stack exec -- ghc-pkg find-module " .. escaped_module .. " --simple-output")

  if not output or output == "" then
    log("Could not find package for module: " .. module_name)
    return nil
  end

  -- ghc-pkg may return multiple package identifiers.
  local package_id = output:match("^(%S+)")

  if package_id then
    log("Resolved package for module: " .. package_id)
  end

  return package_id
end

-- Convert a Haskell module name into the corresponding Hackage
-- documentation filename.
--
-- Example:
--   Data.Aeson.Types -> Data-Aeson-Types
local function module_to_hackage_path(module_name)
  return module_name:gsub("%.", "-")
end

-- Open the Hackage documentation page for a module and entity.
local function open_hackage(module_name, entity_name, package_id, anchor_type)
  if not package_id or package_id == "" then
    package_id = get_package_for_module(module_name)
  end

  if not package_id then
    notify("Could not resolve package for module " .. module_name, vim.log.levels.WARN)
    return
  end

  local module_path = module_to_hackage_path(module_name)

  local url = string.format("https://hackage-content.haskell.org/package/%s/docs/%s.html", package_id, module_path)

  if entity_name and entity_name ~= "" then
    anchor_type = anchor_type or "t"
    url = url .. "#" .. anchor_type .. ":" .. vim.uri_encode(entity_name)
  end

  log("Opening Hackage URL: " .. url)

  vim.ui.open(url)
end

-- Open documentation either for an explicitly supplied qualified name
-- or for the symbol under the cursor.
local function open_hackage_documentation(identifier)
  log_separator("online")

  if identifier and identifier ~= "" then
    -- Expected format:
    --   Data.Aeson.Types.ToJSON
    --
    -- Split the final segment into module and entity.
    local module_name, entity_name = identifier:match("^([%w_%.']+)%.([%w_']+)$")

    if not module_name or not entity_name then
      notify("Expected a qualified name such as Data.Aeson.Types.ToJSON", vim.log.levels.WARN)
      return
    end

    open_hackage(module_name, entity_name, nil)
    return
  end

  -- Resolve the symbol under the cursor using HLS hover information.
  local module_name, package_id, entity_name, hover_text = get_module_from_hover()

  if not module_name then
    notify("Could not resolve symbol from HLS hover. See :HaskellOpenDocsLog", vim.log.levels.WARN)
    return
  end

  local anchor_type = get_haddock_anchor(entity_name, hover_text)

  open_hackage(module_name, entity_name, package_id, anchor_type)
end

-- User command:
--   :HaskellOpenOnlineDocs
--   :HaskellOpenOnlineDocs Data.Aeson.Types.ToJSON
vim.api.nvim_create_user_command(
  "HaskellOpenOnlineDocs",
  function(args) open_hackage_documentation(args.args) end,
  { nargs = "?", desc = "Open online Hackage documentation for the symbol under the cursor" }
)

-- Shortcut to open online documentation for the symbol under the cursor.
vim.keymap.set(
  "n",
  "<leader>hk",
  function() open_hackage_documentation() end,
  { noremap = true, silent = true, buffer = bufnr, desc = "Open online Hackage documentation" }
)

-- View the documentation lookup log.
vim.api.nvim_create_user_command(
  "HaskellOpenDocsLog",
  function() vim.cmd("edit " .. vim.fn.fnameescape(LOG_FILE)) end,
  { desc = "Open the Haskell documentation lookup log" }
)

-- Clear the documentation lookup log.
vim.api.nvim_create_user_command(
  "HaskellOpenDocsClearLog",
  function()
    vim.fn.writefile({}, LOG_FILE)
    notify("Documentation lookup log cleared")
  end,
  { desc = "Clear the Haskell documentation lookup log" }
)



-- ---------------------------------------------------------------------------
-- Local documentation lookup
-- ---------------------------------------------------------------------------

local function find_haddock_module(root, package_id, module_name, module_file)
  local source_file = module_name .. ".html"

  local candidates = {
    -- Root is the package documentation directory.
    {
      path = root .. "/" .. module_file,
      source = false,
    },
    {
      path = root .. "/src/" .. source_file,
      source = true,
    },
    {
      path = root .. "/src/" .. module_file,
      source = true,
    },

    -- Root is the Stack documentation root.
    {
      path = root .. "/" .. package_id .. "/" .. module_file,
      source = false,
    },
    {
      path = root .. "/" .. package_id .. "/src/" .. source_file,
      source = true,
    },
    {
      path = root .. "/" .. package_id .. "/src/" .. module_file,
      source = true,
    },
  }

  for _, candidate in ipairs(candidates) do
    log("Checking Haddock candidate: " .. candidate.path)

    if vim.fn.filereadable(candidate.path) == 1 then
      log("Found local documentation: " .. candidate.path)
      return candidate.path, candidate.source
    end
  end

  return nil, false
end

-- Open local Stack Haddock documentation for a module and entity.
local function open_local_haddock(module_name, entity_name, package_id, anchor_type)
  -- Use the package identifier obtained from HLS hover,
  -- just as open_hackage() does.
  if not package_id or package_id == "" then
    package_id = get_package_for_module(module_name)
  end

  if not package_id then
    notify("Could not resolve package for " .. module_name, vim.log.levels.WARN)
    return
  end

  log("Local Haddock package: " .. package_id)

  -- Haddock converts module dots into hyphens.
  local module_file = module_name:gsub("%.", "-") .. ".html"

  -- Search Stack's documentation roots.
  local roots = {}

  for _, command in ipairs {
    "stack path --local-doc-root",
    "stack path --snapshot-doc-root",
  } do
    local root = run_command(command)

    if root and root ~= "" then
      log("Stack documentation root: " .. root)
      table.insert(roots, root)
    end
  end

  -- Also try the Haddock directory registered in GHC's package database.
  local haddock_root = run_command(
    "stack exec -- ghc-pkg field " .. vim.fn.shellescape(package_id) .. " haddock-html --simple-output"
  )

  if haddock_root and haddock_root ~= "" then
    log("GHC Haddock documentation root: " .. haddock_root)
    table.insert(roots, haddock_root)
  end

  -- Find the module page, including Stack's src/ documentation layout.
  local page = nil
  local is_source = false

  for _, root in ipairs(roots) do
    if root and root ~= "" then
      -- First try the root directly.
      local candidate = root .. "/" .. module_file

      log("Checking Haddock page: " .. candidate)

      if vim.fn.filereadable(candidate) == 1 then
        page = candidate
        break
      end

      -- Then try the package-specific layout, including src/.
      is_source = false

      page, is_source = find_haddock_module(root, package_id, module_name, module_file)

      if page then
        break
      end
    end
  end

  if not page then
    notify(
      "Local Haddock page not found for " .. module_name .. " (" .. package_id .. ")",
      vim.log.levels.WARN
    )
    log("Could not find local Haddock page: " .. module_file)
    return
  end

  -- Source pages and Haddock API pages use different anchors.
  local url = vim.uri_from_fname(page)

  if entity_name and entity_name ~= "" then
    if is_source then
      url = url .. "#" .. vim.uri_encode(entity_name)
    else
      anchor_type = anchor_type or "t"
      url = url .. "#" .. anchor_type .. ":" .. vim.uri_encode(entity_name)
    end
  end

  log("Opening local Haddock URL: " .. url)

  vim.ui.open(url)
end

-- Open local documentation either for an explicitly supplied qualified name
-- or for the symbol under the cursor.
local function open_local_documentation(identifier)
  log_separator("local")

  if identifier and identifier ~= "" then
    -- Example: Data.Aeson.Types.FromJSON
    local module_name, entity_name = identifier:match("^([%w_%.']+)%.([%w_']+)$")

    if not module_name or not entity_name then
      notify("Expected a qualified name such as Data.Aeson.Types.FromJSON", vim.log.levels.WARN)
      return
    end

    open_local_haddock(module_name, entity_name, nil)
    return
  end

  -- Reuse the existing HLS hover resolver.
  local module_name, package_id, entity_name, hover_text = get_module_from_hover()

  if not module_name then
    notify("Could not resolve symbol from HLS hover. See :HaskellOpenDocsLog", vim.log.levels.WARN)
    return
  end

  local anchor_type = get_haddock_anchor(entity_name, hover_text)

  open_local_haddock(module_name, entity_name, package_id, anchor_type)
end

-- User command:
--   :HaskellOpenLocalDocs
--   :HaskellOpenLocalDocs Data.Aeson.Types.FromJSON
vim.api.nvim_create_user_command(
  "HaskellOpenLocalDocs",
  function(args) open_local_documentation(args.args) end,
  { nargs = "?", desc = "Open local Haddock documentation for the symbol under the cursor" }
)

-- Open local Haddock documentation for the symbol under the cursor.
vim.keymap.set(
  "n",
  "<leader>hl",
  function() open_local_documentation() end,
  { noremap = true, silent = true, buffer = bufnr, desc = "Open local Haddock documentation" }
)
