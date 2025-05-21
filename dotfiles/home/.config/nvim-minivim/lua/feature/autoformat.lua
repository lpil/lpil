-- Code auto-formatting

local options = {
	notify_on_error = true,
	format_on_save = {
		timout_ms = 500,
		lsp_format = "fallback",
	},
	formatters_by_ft = {
		lua = { "stylua" },
	},
}

local format = function()
	require("conform").format({
		async = true,
		lsp_format = "fallback",
	})
end

local setup_keymaps = function()
	vim.keymap.set("n", "<leader>f", format, {
		desc = "[F]ormat buffer",
	})
end

return {
	add = "stevearc/conform.nvim",
	later = function()
		require("conform").setup(options)
		setup_keymaps()
	end,
	ensure_installed = {
		"stylua",
	},
}
