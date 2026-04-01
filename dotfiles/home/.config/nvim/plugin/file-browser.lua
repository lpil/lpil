require("mini.files").setup({
  windows = {
    preview = true,
    width_focus = 30,
    width_preview = 30,
  },
  options = {
    use_as_default_explorer = true,
  },
})

local open_minifiles = function()
  local mini = require("mini.files")
  mini.open(vim.api.nvim_buf_get_name(0), true)
  mini.reveal_cwd()
end
vim.keymap.set("n", "<leader>e", open_minifiles, { desc = "Open mini.files" })
