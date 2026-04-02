require("telescope").setup({
  extensions = {
    ["ui-select"] = {
      require("telescope.themes").get_dropdown(),
    },
  },
})

-- Enable Telescope extensions if they are installed
pcall(require("telescope").load_extension, "fzf")
pcall(require("telescope").load_extension, "ui-select")

-- See `:help telescope.builtin`
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader><leader>", builtin.find_files, { desc = "Files" })
vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "Help" })
vim.keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "Keymaps" })
vim.keymap.set("n", "<leader>ss", builtin.builtin, { desc = "Telescopes" })
vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "Grep current word" })
vim.keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "Grep" })
vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "Diagnostics" })
vim.keymap.set("n", "<leader>sc", builtin.resume, { desc = "Continue previous search" })
vim.keymap.set("n", "<leader>sr", builtin.oldfiles, { desc = 'Recent files' })
vim.keymap.set("n", "<leader>sb", builtin.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>/", builtin.current_buffer_fuzzy_find, { desc = "Search current buffer" })

-- It's also possible to pass additional configuration options.
--  See `:help telescope.builtin.live_grep()` for information about particular keys
vim.keymap.set("n", "<leader>s/", function()
  builtin.live_grep({
    grep_open_files = true,
    prompt_title = "Live Grep in Open Files",
  })
end, { desc = "Search in open files" })

vim.keymap.set("n", "<leader>sn", function()
  builtin.find_files({ cwd = vim.fn.stdpath("config") })
end, { desc = "Search neovim config" })

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("picker-lsp-attach", { clear = true }),
  callback = function(event)
    -- In this case, we create a function that lets us more easily define mappings specific
    -- for LSP related items. It sets the mode, buffer and description for us each time.
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = desc })
    end

    -- Execute a code action, usually your cursor needs to be on top of an error
    -- or a suggestion from your LSP for this to activate.
    map("gra", vim.lsp.buf.code_action, "Goto code action", { "n", "x" })

    -- Find references for the word under your cursor.
    map("grr", require("telescope.builtin").lsp_references, "Goto references")

    -- Jump to the implementation of the word under your cursor.
    --  Useful when your language has ways of declaring types without an actual implementation.
    map("gri", require("telescope.builtin").lsp_implementations, "Goto implementation")

    -- Jump to the definition of the word under your cursor.
    --  This is where a variable was first declared, or where a function is defined, etc.
    --  To jump back, press <C-t>.
    map("gd", require("telescope.builtin").lsp_definitions, "Goto definition")

    -- This is not Goto Definition, this is Goto Declaration.
    -- For example, in C this would take you to the header.
    map("gD", vim.lsp.buf.declaration, "Goto declaration")

    -- Fuzzy find all the symbols in your current document.
    --  Symbols are things like variables, functions, types, etc.
    map("gO", require("telescope.builtin").lsp_document_symbols, "Open document symbols")

    -- Fuzzy find all the symbols in your current workspace.
    --  Similar to document symbols, except searches over your entire project.
    map("gW", require("telescope.builtin").lsp_dynamic_workspace_symbols, "Open workspace symbols")

    -- Jump to the type of the word under your cursor.
    --  Useful when you're not sure what type a variable is and you want to see
    --  the definition of its *type*, not where it was *defined*.
    map("grt", require("telescope.builtin").lsp_type_definitions, "Goto type definition")
  end
})
