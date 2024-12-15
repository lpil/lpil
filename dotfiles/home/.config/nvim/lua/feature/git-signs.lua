local options = {
	signs = {
		add = { text = "+" },
		change = { text = "~" },
		delete = { text = "_" },
		topdelete = { text = "‾" },
		changedelete = { text = "~" },
	},
}

return {
	add = "lewis6991/gitsigns.nvim",
	now = function()
		require("gitsigns").setup(config)
	end,
}
