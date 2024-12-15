return {
	add = {
		source = "folke/todo-comments.nvim",
		depends = { "nvim-lua/plenary.nvim" },
	},
	later = function()
		require("todo-comments").setup({ signs = false })
	end,
}
