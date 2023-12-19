return {
  { "lukas-reineke/indent-blankline.nvim", enabled = false },

  {
    "echasnovski/mini.indentscope/",
    opts = {
      draw = {
        animation = function()
          return 5
        end,
      },
    },
  },
}
