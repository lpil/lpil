vim.g.have_nerd_font = true

vim.o.number = false
vim.o.relativenumber = false
vim.o.mouse = "a"
vim.o.showmode = false

--  Schedule the setting after `UiEnter` because it can increase startup-time.
vim.schedule(function()
  vim.o.clipboard = "unnamedplus"
end)

vim.o.breakindent = true
vim.o.undofile = true

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.signcolumn = "yes"

vim.o.updatetime = 250
vim.o.timeoutlen = 300

vim.o.splitright = true
vim.o.splitbelow = true

-- Set indentation defaults for all filetypes
vim.opt.tabstop = 4      -- Show tabs as 4 spaces
vim.opt.shiftwidth = 2   -- Indent using 2 spaces
vim.opt.softtabstop = 2  -- Number of spaces <Tab> counts for in insert mode
vim.opt.expandtab = true -- Use spaces instead of tabs

--  Notice listchars is set using `vim.opt` instead of `vim.o`.
--  It is very similar to `vim.o` but offers an interface for conveniently interacting with tables.
--   See `:help lua-options`
--   and `:help lua-options-guide`
vim.o.list = true
vim.opt.listchars = { tab = "  ", trail = "·", nbsp = "␣" }

vim.o.inccommand = "split"
vim.o.cursorline = true

vim.o.scrolloff = 10
vim.o.confirm = true

vim.o.showtabline = 2

-- Enable backups
vim.o.backup = true
vim.o.writebackup = true
vim.o.swapfile = true
vim.o.undofile = true
local data_dir = vim.fn.stdpath('data')
local backup_dir = data_dir .. '/backup//'
local swap_dir = data_dir .. '/swap//'
local undo_dir = data_dir .. '/undo//'
vim.o.backupdir = backup_dir
vim.o.directory = swap_dir
vim.o.undodir = undo_dir
vim.fn.mkdir(backup_dir, 'p')
vim.fn.mkdir(swap_dir, 'p')
vim.fn.mkdir(undo_dir, 'p')

