return {
  {
    "lukas-reineke/indent-blankline.nvim",
    enabled = false,
  },

  {
    "echasnovski/mini.indentscope",
    opts = {
      draw = {
        delay = 100,
        animation = function()
          return 20
        end,
      },
    },
  },
}
