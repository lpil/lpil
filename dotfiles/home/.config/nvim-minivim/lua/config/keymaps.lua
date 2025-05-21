return {
	setup = function()
		vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

		vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, {
			desc = "Open quickfix",
		})

		vim.keymap.set("t", "<Esc>", "<C-\\><C-n>", {
			desc = "Exit terminal mode",
		})

		vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Focus left window" })
		vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Focus right window" })
		vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Focus lower window" })
		vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Focus upper window" })

		vim.api.nvim_create_autocmd("TextYankPost", {
			desc = "Highlight when yanking text",
			group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
			callback = function()
				vim.highlight.on_yank()
			end,
		})
	end,
}
