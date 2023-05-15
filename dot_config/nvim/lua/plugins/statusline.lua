return {
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      { 'nvim-tree/nvim-web-devicons' },
      { 'SmiteshP/nvim-navic' }
    },
    opts = function()
      local navic = require("nvim-navic")

      return {
        disabled_filetypes = {
          statusline = {
          },
          winbar = {
          },
        },
        extensions = {
          'neo-tree',
          'aerial',
          'fugitive',
          'trouble',
          'lazy',
        },
        winbar = {
          lualine_c = {
            {
              function()
                return navic.get_location()
              end,
              cond = function()
                return navic.is_available()
              end
            },
          }
        },
        inactive_winbar = {
          lualine_c = {
            {
              function()
                return navic.get_location()
              end,
              cond = function()
                return navic.is_available()
              end
            },
          }
        },
        sections = {
          lualine_c = {
            { 'filename', file_status = true, path = 1 },
          }
        },
      }
    end,
  }
}
