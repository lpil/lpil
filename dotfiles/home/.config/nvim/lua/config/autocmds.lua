-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- Disable auto formatting on save for some filetypes
vim.api.nvim_create_autocmd({ "FileType" }, {
  pattern = { "markdown", "yaml" },
  callback = function()
    vim.b.autoformat = false
  end,
})
