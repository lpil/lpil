-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- LazyVim sets these to move lines up and down, however on my mechanical
-- keyboard I keep triggering it accidentally when tapping esc and then
-- navigating. I don't use these, so disable them.
vim.keymap.del("n", "<A-j>")
vim.keymap.del("n", "<A-k>")
vim.keymap.del("i", "<A-j>")
vim.keymap.del("i", "<A-k>")
vim.keymap.del("v", "<A-j>")
vim.keymap.del("v", "<A-k>")

--
-- Replace bnext and bprevious with a version that skips over buffers that are
-- open in other windows.
--

local function next_non_open_buffer(direction)
  local initial = vim.fn.bufnr("%")

  while true do
    -- Move to the next buffer
    vim.cmd(direction)
    local buffer_number = vim.fn.bufnr("%")

    local windows = vim.fn.win_findbuf(buffer_number)
    local open_twice = #windows > 1
    if not open_twice then
      -- We've found a buffer that is not already open. Victory!
      break
    end

    if buffer_number == initial then
      -- We've looped around to the initial buffer, meaning there are no
      -- buffers that are not already open. Give up.
      break
    end
  end
end

vim.keymap.set("n", "L", function()
  next_non_open_buffer("bnext")
end)

vim.keymap.set("n", "H", function()
  next_non_open_buffer("bprevious")
end)

vim.keymap.set("n", "<leader>ci", function()
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }))
end, { desc = "Toggle inlay hints" })

vim.keymap.set("t", "<esc>", "<c-\\><c-n>")
