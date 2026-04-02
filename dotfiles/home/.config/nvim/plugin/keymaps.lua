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

vim.keymap.set("n", "L", function() next_non_open_buffer("bnext") end,
  { desc = "Next buffer" })
vim.keymap.set("n", "H", function() next_non_open_buffer("bprevious") end,
  { desc = "Previous buffer" })

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

local function close_buffer()
  local window_count = #vim.api.nvim_tabpage_list_wins(0)

  -- Avoid closing the split when closing the buffer
  if window_count > 1 then
    vim.cmd('bnext')
    vim.cmd('bdelete #')
  else
    vim.cmd('bdelete')
  end
end

local function write_and_quit()
  vim.cmd('wqa!')
end

vim.keymap.set("n", "<leader>w", ":w<cr>", { desc = "Write buffer" })
vim.keymap.set('n', '<leader>bd', close_buffer, { desc = 'Close buffer' })
vim.keymap.set("n", "<leader>qq", write_and_quit, { desc = "Write all and quit" })

-- Better indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- Diagnostic keymaps
local error_jump = function(offset)
  vim.diagnostic.jump({
    severity = vim.diagnostic.severity.ERROR,
    count = offset,
    float = true
  })
end
vim.keymap.set("n", "]e", function() error_jump(1) end,
  { desc = "Next error diagnostic" })
vim.keymap.set("n", "[e", function() error_jump(-1) end,
  { desc = "Previous error diagnostic" })

vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist,
  { desc = "Open diagnostic quickfix list" })

-- Default is <C-\><C-n>
vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })
vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })
