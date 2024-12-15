return {
	now = function()
		vim.opt.showmode = false
		require("mini.git").setup()
		require("mini.statusline").setup()
	end,
}
