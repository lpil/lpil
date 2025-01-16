local config = {
	windows = {
		preview = true,
		width_focus = 30,
		width_preview = 30,
	},
	options = {
		use_as_default_explorer = true,
	},
}

local setup_lsp_rename_autocmd = function()
	vim.api.nvim_create_autocmd("User", {
		pattern = "MiniFilesActionRename",
		callback = function(event)
			-- TODO:: LSP rename event
			require("lazyvim.util").lsp.on_rename(event.data.from, event.data.to)
		end,
	})
end

local open = function()
	local mini = require("mini.files")
	mini.open(vim.api.nvim_buf_get_name(0), true)
	mini.reveal_cwd()
end

local setup_keybinds = function()
	vim.keymap.set("n", "<leader>e", open, {
		desc = "Open mini.files (directory of current file)",
	})
end

return {
	add = "echasnovski/mini.files",
	later = function()
		require("mini.files").setup(config)
		setup_keybinds()
		setup_lsp_rename_autocmd()
	end,
}
