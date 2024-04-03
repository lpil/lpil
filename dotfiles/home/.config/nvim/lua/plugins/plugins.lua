return {
  -- { import = "lazyvim.plugins.extras.coding.copilot" },
  { import = "lazyvim.plugins.extras.editor.leap" },
  { import = "lazyvim.plugins.extras.lang.json" },
  { "fffnite/gleam-theme-nvim" },

  {
    "neovim/nvim-lspconfig",
    ---@class PluginLspOpts
    opts = {
      servers = {
        gleam = { mason = false },
        rust_analyzer = { mason = false },
        denols = { mason = false },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, {
        "gleam",
        "typescript",
        "elixir",
      })
    end,
  },

  -- add any tools you want to have installed below
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        -- "stylua",
        -- "shellcheck",
        -- "shfmt",
        -- "flake8",
      },
    },
  },
}
