require('mini.completion').setup()

-- Make <c-y> auto-select the first item is nothing is selected
vim.keymap.set("i", "<c-y>", function()
  local info = vim.fn.complete_info({ "selected" })
  local completing = vim.fn.pumvisible() ~= 0
  local none_selected = info.selected == -1

  if completing and none_selected then
    return "<C-n><C-y>"
  end
  return "<C-y>"
end, { expr = true, replace_keycodes = true })

