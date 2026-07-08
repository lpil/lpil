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

-- Edit surrounding such quotes
require("mini.surround").setup()

-- Show buffers as tabs
require("mini.tabline").setup()

-- Better f command
require('mini.jump').setup()
