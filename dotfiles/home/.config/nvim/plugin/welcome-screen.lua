vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    if vim.fn.argc() == 0 then
      vim.cmd("enew")
      local lines = {
        "Welcome back, commander.",
      }
      local centered_lines = {}
      local width = vim.api.nvim_win_get_width(0)
      local height = vim.api.nvim_win_get_height(0)

      for _, line in ipairs(lines) do
        local padding = math.floor((width - vim.api.nvim_strwidth(line)) / 2)
        if line == "" or padding <= 0 then
          table.insert(centered_lines, line)
        else
          table.insert(centered_lines, string.rep(" ", padding) .. line)
        end
      end

      local top_padding = math.floor((height - #centered_lines * 2) / 2)
      for _ = 1, top_padding do
        table.insert(centered_lines, 1, "")
      end

      vim.api.nvim_buf_set_lines(0, 0, -1, false, centered_lines)
      local buf = vim.api.nvim_get_current_buf()
      vim.bo[buf].buftype = "nofile"
      vim.bo[buf].bufhidden = "hide"
      vim.bo[buf].swapfile = false
      vim.bo[buf].modifiable = false
      vim.bo[buf].readonly = true
      vim.bo[buf].filetype = "dashboard"
      vim.bo[buf].buflisted = false
    end
  end
})
