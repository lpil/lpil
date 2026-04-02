vim.pack.add({
  -- Colourscheme
  'https://github.com/EdenEast/nightfox.nvim',

  -- Language server configurations
  'https://github.com/neovim/nvim-lspconfig',

  -- Tree sitter grammars for syntax highlighting
  'https://github.com/nvim-treesitter/nvim-treesitter',

  -- Lots of nice little plugins!
  'https://github.com/nvim-mini/mini.nvim',

   -- Detect tabstop and shiftwidth automatically
  'https://github.com/NMAC427/guess-indent.nvim',

  -- Adds git related signs to the gutter
  'https://github.com/lewis6991/gitsigns.nvim',

  -- Notification and progress UI
  'https://github.com/j-hui/fidget.nvim',

  -- Picker UI
  'https://github.com/nvim-telescope/telescope.nvim',
  'https://github.com/nvim-lua/plenary.nvim',
  'https://github.com/nvim-telescope/telescope-ui-select.nvim',
})

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Colourscheme and styling
require("nightfox").setup()
require('mini.icons').setup()
require("mini.statusline").setup({ use_icons = vim.g.have_nerd_font })
vim.cmd.colorscheme("duskfox")

-- Auto-close parens, etc
require("mini.pairs").setup()

-- Show buffers as tabs
require("mini.tabline").setup()

-- Better f command
require('mini.jump').setup()

--
-- vim.api.nvim_create_autocmd("LspAttach", {
--   group = vim.api.nvim_create_augroup("lpil-lsp-attach", { clear = true }),
--   callback = function(event)
--     -- The following two autocommands are used to highlight references of the
--     -- word under your cursor when your cursor rests there for a little while.
--     --    See `:help CursorHold` for information about when this is executed
--     --
--     -- When you move your cursor, the highlights will be cleared (the second autocommand).
--     local client = vim.lsp.get_client_by_id(event.data.client_id)
--     if
--       client
--       and client:supports_method(vim.lsp.protocol.Methods.textDocument_documentHighlight, event.buf)
--     then
--       local highlight_augroup =
--       vim.api.nvim_create_augroup("kickstart-lsp-highlight", { clear = false })
--       vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
--         buffer = event.buf,
--         group = highlight_augroup,
--         callback = vim.lsp.buf.document_highlight,
--       })
--
--       vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
--         buffer = event.buf,
--         group = highlight_augroup,
--         callback = vim.lsp.buf.clear_references,
--       })
--
--       vim.api.nvim_create_autocmd("LspDetach", {
--         group = vim.api.nvim_create_augroup("kickstart-lsp-detach", { clear = true }),
--         callback = function(event2)
--           vim.lsp.buf.clear_references()
--           vim.api.nvim_clear_autocmds({ group = "kickstart-lsp-highlight", buffer = event2.buf })
--         end,
--       })
--     end
--
--     -- The following code creates a keymap to toggle inlay hints in your
--     -- code, if the language server you are using supports them
--     --
--     -- This may be unwanted, since they displace some of your code
--     if
--       client
--       and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf)
--     then
--       map("<leader>th", function()
--         vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }))
--       end, "[T]oggle Inlay [H]ints")
--     end
--   end,
-- })

-- require("lazy").setup({
--
--   -- LSP Plugins
--   {
--     -- Main LSP Configuration
--     "neovim/nvim-lspconfig",
--     dependencies = {
--     },
--     config = function()
--       --  This function gets run when an LSP attaches to a particular buffer.
--       --    That is to say, every time a new file is opened that is associated with
--       --    an lsp (for example, opening `main.rs` is associated with `rust_analyzer`) this
--       --    function will be executed to configure the current buffer
--
--
--       -- Diagnostic Config
--       -- See :help vim.diagnostic.Opts
--       vim.diagnostic.config({
--         severity_sort = true,
--         float = { border = "rounded", source = "if_many" },
--         underline = { severity = vim.diagnostic.severity.ERROR },
--         signs = vim.g.have_nerd_font and {
--           text = {
--             [vim.diagnostic.severity.ERROR] = "󰅚 ",
--             [vim.diagnostic.severity.WARN] = "󰀪 ",
--             [vim.diagnostic.severity.INFO] = "󰋽 ",
--             [vim.diagnostic.severity.HINT] = "󰌶 ",
--           },
--         } or {},
--         virtual_text = {
--           source = "if_many",
--           current_line = false,
--           spacing = 2,
--           format = function(diagnostic)
--             -- Collapse whitespace
--             local collapsed, _ = diagnostic.message:gsub("%s%s+", "  ")
--             return collapsed
--           end,
--         },
--       })
--     end,
--   },
--
--   { -- Collection of various small independent plugins/modules
--     "echasnovski/mini.nvim",
--     config = function()
--       -- Better Around/Inside textobjects
--       --
--       -- Examples:
--       --  - va)  - [V]isually select [A]round [)]paren
--       --  - yinq - [Y]ank [I]nside [N]ext [Q]uote
--       --  - ci'  - [C]hange [I]nside [']quote
--       require("mini.ai").setup({ n_lines = 500 })
--
--       -- Add/delete/replace surroundings (brackets, quotes, etc.)
--       --
--       -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
--       -- - sd'   - [S]urround [D]elete [']quotes
--       -- - sr)'  - [S]urround [R]eplace [)] [']
--       require("mini.surround").setup()
--     end,
--   },
