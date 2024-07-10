local autocolor = require("config.plugins.autocolor")

autocolor.set_bg()

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
        hl.DiagnosticUnnecessary = { fg = "#707880" }

        -- Telescope
        hl.TelescopeNormal       = { fg = c.fg, bg = c.bg }
        hl.TelescopeBorder       = { fg = c.border_highlight, bg = c.bg }
        hl.TelescopePromptBorder = { fg = c.orange, bg = c.bg }
        hl.TelescopePromptTitle  = { fg = c.orange, bg = c.bg }

        -- Flash
        hl.FlashLabel            = { fg = c.orange, bg = c.bg }

        -- Float
        hl.NormalFloat           = { bg = c.bg }
        hl.FloatBorder           = { fg = c.border_highlight, bg = c.bg }
      end
    },
    init = function()
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
}
