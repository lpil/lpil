-- Enable visual wrap
vim.opt_local.wrap = true

-- Wrap at word boundaries, not mid-word
vim.opt_local.linebreak = true

-- Indent wrapped lines to match the start of the original line
vim.opt_local.breakindent = true
vim.opt_local.showbreak = "↪ "

-- 2 space indentation
vim.opt_local.shiftwidth = 2
vim.opt_local.tabstop = 2
vim.opt_local.softtabstop = 2

-- Spell checking
vim.opt_local.spell = true
