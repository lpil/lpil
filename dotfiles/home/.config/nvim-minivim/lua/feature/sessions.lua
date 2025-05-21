return {
	add = "rmagatti/auto-session",
	later = function()
		vim.o.sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions"
		require("auto-session").setup({
			auto_restore = false,
		})
	end,
}
