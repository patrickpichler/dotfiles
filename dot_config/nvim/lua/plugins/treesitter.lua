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
    "nvim-treesitter/nvim-treesitter",

    event = { "VeryLazy" },

    dependencies = {
      {
        "nvim-treesitter/nvim-treesitter-textobjects",
      }
    },
    build = ":TSUpdate",
    opts = function()
      return {
        ensure_installed = {
          "c", "lua", "vim", "vimdoc", "elixir", "javascript", "html", "zig", "go", "templ", "css", "nix",
          "xml", "bash", "diff", "http", "java", "make", "rust", "toml", "yaml", "gomod", "json5", "proto", "templ",
          "kotlin", "python", "svelte", "vimdoc", "comment", "clojure", "markdown", "starlark",
        },
        sync_install = true,
        ignore_install = { "phpdoc" },
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
        indent = { enable = true },
        autotag = { enable = true },
        textobjects = {
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
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<cr>",
            node_incremental = "<cr>",
            scope_incremental = false,
            node_decremental = "<bs>",
          },
        },
      }
    end,
    config = function(_, opts)
      -- require("nvim-treesitter.install").prefer_git = true
      require("nvim-treesitter.configs").setup(opts)
    end
  },
}
