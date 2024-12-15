return {
	add = { source = "folke/todo-comments.nvim", "VimEnter", depends = { "nvim-lua/plenary.nvim" } },
	later = function()
		require("todo-comments").setup({ signs = false })
	end,
}
