return {
  { "rose-pine/neovim", name = "rose-pine" },

  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "night",

      on_highlights = function(hl, c, w)
        hl.DiagnosticUnnecessary = { fg = "#707880"}
      end
    },
    init = function()
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
}
