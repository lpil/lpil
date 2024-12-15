local function next_non_open_buffer(direction)
	local initial = vim.fn.bufnr("%")

	while true do
		-- Move to the next buffer
		vim.cmd(direction)
		local buffer_number = vim.fn.bufnr("%")

		local windows = vim.fn.win_findbuf(buffer_number)
		local open_twice = #windows > 1
		if not open_twice then
			-- We've found a buffer that is not already open. Victory!
			break
		end

		if buffer_number == initial then
			-- We've looped around to the initial buffer, meaning there are no
			-- buffers that are not already open. Give up.
			break
		end
	end
end

return {
	now = function()
		require("mini.icons").setup()
		require("mini.tabline").setup()
	end,
	later = function()
		vim.keymap.set("n", "L", function()
			next_non_open_buffer("bnext")
		end)

		vim.keymap.set("n", "H", function()
			next_non_open_buffer("bprevious")
		end)
	end,
}
