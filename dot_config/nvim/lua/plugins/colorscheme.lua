return {
  {
    'catppuccin/nvim',
    name = 'catppucin',

    priority = 1000,

    config = function(_, opts)
      require('catppuccin').setup(opts)

      vim.cmd.colorscheme 'catppuccin'
    end,

    opts = {
      flavour = "latte", -- latte, frappe, macchiato, mocha
      background = {
        light = "latte",
        dark = "mocha",
      },
      transparent_background = false,
      show_end_of_buffer = true, -- show the '~' characters after the end of buffers
      term_colors = false,
      dim_inactive = {
        enabled = false,
        shade = "dark",
        percentage = 0.15,
      },
      no_italic = false, -- Force no italic
      no_bold = false,   -- Force no bold
      styles = {
        -- comments = { "italic" },
        -- conditionals = { "italic" },
        -- loops = {},
        -- functions = {},
        -- keywords = {},
        -- strings = {},
        -- variables = {},
        -- numbers = {},
        -- booleans = {},
        -- properties = {},
        -- types = {},
        -- operators = {},
      },
      -- color_overrides = {},
      custom_highlights = function(C)
        return {
        }
      end,
      integrations = {
        cmp = true,
        fidget = true,
        gitsigns = true,
        illuminate = true,
        lsp_trouble = true,
        notify = true,
        noice = true,
        mason = true,
        markdown = true,
        neotree = true,
        native_lsp = {
          enabled = true,
        },
        sandwich = true,
        telescope = true,
        indent_blankline = {
          enabled = true,
        },
        aerial = true,
        dap = {
          enabled = true,
          enabled_ui = true,
        },
        -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
      },
    }
  }
}
