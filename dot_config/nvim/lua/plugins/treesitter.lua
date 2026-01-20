return {
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "BufReadPre",
    opts = {
      enable = true,            -- Enable this plugin (Can be enabled/disabled later via commands)
      max_lines = 10,           -- How many lines the window should span. Values <= 0 mean no limit.
      min_window_height = 0,    -- Minimum editor window height to enable context. Values <= 0 mean no limit.
      line_numbers = true,
      multiline_threshold = 20, -- Maximum number of lines to collapse for a single context line
      trim_scope = "outer",     -- Which context lines to discard if `max_lines` is exceeded. Choices: "inner", "outer"
      mode = "cursor",          -- Line used to calculate context. Choices: "cursor", "toplene"
      -- Separator between context and content. Should be a sengle character string, like "-".
      -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
      separator = nil,
      zindex = 20, -- The Z-index of the context window
    },
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    opts = {
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          ["]m"] = "@function.outer",
          ["]c"] = { query = "@class.outer", desc = "Next class start" },
          ["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
        },
        goto_next_end = {
          ["]M"] = "@function.outer",
        },
        goto_previous_start = {
          ["[m"] = "@function.outer",
          ["[c"] = "@class.outer",
        },
        goto_previous_end = {
          ["[M"] = "@function.outer",
        },
      },
    }
  },

  {
    "nvim-treesitter/nvim-treesitter",

    lazy = false,

    dependencies = {
      {
        "nvim-treesitter/nvim-treesitter-textobjects",
      }
    },
    build = ":TSUpdate",
    opts = {},

    config = function(_, opts)
      require("nvim-treesitter.config").setup(opts)

      -- Install wanted and wait for three minutes
      require('nvim-treesitter').install(
        { "c", "lua", "vim", "vimdoc", "elixir", "javascript", "html", "zig", "go", "templ", "css", "nix",
          "xml", "bash", "diff", "http", "java", "make", "rust", "toml", "yaml", "gomod", "json5", "proto", "templ",
          "kotlin", "python", "svelte", "vimdoc", "comment", "clojure", "markdown", "starlark",
          "yaml", "helm" }
      ):wait(300)

      vim.api.nvim_create_autocmd('FileType', {
        pattern = '*',
        callback = function()
          local installed = require('nvim-treesitter').get_available()

          if not vim.tbl_contains(installed, vim.bo.filetype) then
            return
          end

          vim.treesitter.start()

          vim.wo[0][0].foldexpr = 'v:lua.vim.treesitter.foldexpr()'
          vim.wo[0][0].foldmethod = 'expr'

          vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end
      })
    end
  },
}
