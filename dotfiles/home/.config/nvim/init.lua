vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("config/options").setup()
require("config/keymaps").setup()
require("bootstrap/minideps").setup()
require("bootstrap/mason").setup()

-- Conventional modules are modules which may export certain functions
-- `add`: Value to call `MiniDeps.add` with
-- `now`: Value to call `MiniDeps.now` with
-- `later`: Value to call `MiniDeps.later` with
-- `ensure_installed`: Value to use with `mason-tool-installer.ensure_installed`
local conventional_modules = {
	"feature/theme",
	"feature/git-signs",
	"feature/autoformat",
	"feature/file-tree",
	"feature/keybind-hints", -- Ideally last
}

local ensure_installed = {}

for _, module in ipairs(conventional_modules) do
	local m = require(module)
	if m.add then
		MiniDeps.add(m.add)
	end
	if m.now then
		MiniDeps.now(m.now)
	end
	if m.later then
		MiniDeps.later(m.later)
	end
	if m.ensure_installed then
		table.insert(ensure_installed, m.ensure_installed)
	end
end

MiniDeps.later(function()
	require("mason-tool-installer").setup({ ensure_installed = ensure_installed })
end)
