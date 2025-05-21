return {
	later = function()
		require("mini.diff").setup({
			view = {
				signs = {
					add = "+",
					change = "~",
					delete = "-",
				},
			},
		})
	end,
}
