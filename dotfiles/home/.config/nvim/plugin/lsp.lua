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

-- Highlight references
vim.api.nvim_set_hl(0, "LspReferenceText",  { link = "Search" })
vim.api.nvim_set_hl(0, "LspReferenceRead",  { link = "Search" })
vim.api.nvim_set_hl(0, "LspReferenceWrite", { link = "Search" })

-- Highlight references, then clear on next keypress
vim.keymap.set("n", "grh", function()
  vim.lsp.buf.document_highlight()

  -- Create a one-shot autocmd to clear on next keypress
  local group = vim.api.nvim_create_augroup("lsp_highlight_once", { clear = true })

  vim.api.nvim_create_autocmd("CursorMoved", {
    group = group,
    once = true,
    callback = function()
      vim.lsp.buf.clear_references()
    end,
  })

  vim.api.nvim_create_autocmd("InsertEnter", {
    group = group,
    once = true,
    callback = function()
      vim.lsp.buf.clear_references()
    end,
  })
end, { desc = "LSP highlight references (until next move)" })
