return {
  { "rose-pine/neovim", name = "rose-pine" },

  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "night",
      light_style = "day",

      on_highlights = function(hl, c)
        hl.DiagnosticUnnecessary = { fg = "#707880"}
      end
    },
    init = function()
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
}
