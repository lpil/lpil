local format = function()
  vim.lsp.buf.format({ async = false })
end

vim.keymap.set("n", "<leader>f", format, { desc = "Format buffer" })

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("lsp", { clear = true }),
  callback = function(args)
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("lsp_format_" .. args.buf, { clear = true }),
      buffer = args.buf,
      callback = format,
    })
  end
})
