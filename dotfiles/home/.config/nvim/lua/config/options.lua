return {
	setup = function()
		vim.opt.number = false
		vim.opt.mouse = "a"

		vim.opt.expandtab = true
		vim.opt.shiftwidth = 2
		vim.opt.tabstop = 2

		vim.opt.showmode = true

		--  Schedule the setting after `UiEnter` because it can increase startup-time.
		vim.schedule(function()
			vim.opt.clipboard = "unnamedplus"
		end)

		vim.opt.breakindent = true
		vim.opt.undofile = true

		vim.opt.ignorecase = true
		vim.opt.smartcase = true

		vim.opt.signcolumn = "yes"

		vim.opt.updatetime = 250
		vim.opt.timeoutlen = 300

		vim.opt.splitright = true
		vim.opt.splitbelow = true

		vim.opt.list = true
		vim.opt.listchars = { tab = "  ", trail = "·", nbsp = "␣" }

		vim.opt.inccommand = "nosplit"

		vim.opt.cursorline = true
		vim.opt.scrolloff = 10
	end,
}
