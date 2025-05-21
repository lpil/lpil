return {
	setup = function()
		MiniDeps.add("williamboman/mason.nvim")
		MiniDeps.add("williamboman/mason-lspconfig.nvim")
		MiniDeps.add("WhoIsSethDaniel/mason-tool-installer.nvim")
		MiniDeps.later(function()
			require("mason").setup()
		end)
	end,
}
