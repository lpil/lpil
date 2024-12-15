local buffer = vim.api.nvim_get_current_buf()

vim.keymap.set("n", "<leader><leader>x", "<cmd>source %<cr>", {
	desc = "Source current file",
	buffer = buffer,
})

vim.keymap.set("n", "<leader>x", ":.lua<cr>", {
	desc = "Source current line",
	buffer = buffer,
})

vim.keymap.set("v", "<leader>x", ":lua<cr>", {
	desc = "Source current selection",
	buffer = buffer,
})
