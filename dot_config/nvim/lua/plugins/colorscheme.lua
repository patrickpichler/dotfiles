local autocolor = require("config.plugins.autocolor")

autocolor.set_bg()

return {
  {
    "rose-pine/neovim",
    name = "rose-pine",
    init = function()
      vim.cmd([[colorscheme rose-pine]])
    end,
  },

  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "night",
      light_style = "day",

      on_highlights = function(hl, c)
        hl.DiagnosticUnnecessary = { fg = "#707880" }

        -- Flash
        hl.FlashLabel            = { fg = c.orange, bg = c.bg }

        -- Float
        hl.NormalFloat           = { bg = c.bg }
        hl.FloatBorder           = { fg = c.border_highlight, bg = c.bg }
      end
    },
  },
}
