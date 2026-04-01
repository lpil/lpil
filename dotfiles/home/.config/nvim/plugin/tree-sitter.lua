local ts = require("nvim-treesitter")

local languages = {
  "bash",
  "diff",
  "djot",
  "elixir",
  "erlang",
  "gleam",
  "html",
  "javascript",
  "lua",
  "luadoc",
  "markdown",
  "markdown_inline",
  "query",
  "rust",
  "typescript",
  "vim",
  "vimdoc",
  "yaml",
}

ts.install(languages, { summary = false })

vim.api.nvim_create_autocmd("FileType", {
  pattern = languages,
  callback = function()
    -- Syntax highlighting, provided by Neovim
    vim.treesitter.start()
    -- Folds, provided by Neovim
    vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
    -- Indentation, provided by nvim-treesitter
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end,
})
