vim.lsp.enable({
  "gleam",
  "rust_analyzer",
  "ts_ls",
})

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, args.buf, {
        autotrigger = true, -- triggers automatically as you type
      })
    end
  end,
})
