return {
  {
    "nvim-treesitter/playground",
    cmd = "TSPlaygroundToggle"
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "BufReadPre",
    opts = {
      enable = true,            -- Enable this plugin (Can be enabled/disabled later via commands)
      max_lines = 0,            -- How many lines the window should span. Values <= 0 mean no limit.
      min_window_height = 0,    -- Minimum editor window height to enable context. Values <= 0 mean no limit.
      line_numbers = true,
      multiline_threshold = 20, -- Maximum number of lines to collapse for a single context line
      trim_scope = 'outer',     -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
      mode = 'cursor',          -- Line used to calculate context. Choices: 'cursor', 'topline'
      -- Separator between context and content. Should be a single character string, like '-'.
      -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
      separator = nil,
      zindex = 20, -- The Z-index of the context window
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",

    event = { 'VeryLazy' },

    dependencies = {
      {
        "HiPhish/nvim-ts-rainbow2",
      }
    },
    make = ":TSUpdate",
    opts = function()
      return {
        ensure_installed = "all",
        ignore_install = { "phpdoc" },
        highlight = { enable = true },
        indent = { enable = true },
        autotag = { enable = true },
        rainbow = {
          enable = true,
          -- list of languages you want to disable the plugin for
          disable = { 'jsx', 'cpp' },
          -- Which query to use for finding delimiters
          query = 'rainbow-parens',
          -- Highlight the entire buffer all at once
          strategy = require('ts-rainbow').strategy.global,
        }
      }
    end,
    config = function(_, opts)
      -- require("nvim-treesitter.install").prefer_git = true
      require('nvim-treesitter.configs').setup(opts)
    end
  },

  {
    "mfussenegger/nvim-treehopper",
    keys = { { "m", mode = { "o", "x" } } },
    config = function()
      vim.cmd([[
        omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>
        xnoremap <silent> m :lua require('tsht').nodes()<CR>
      ]])
    end,
  },

}
