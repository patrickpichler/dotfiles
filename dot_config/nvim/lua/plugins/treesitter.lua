local function textobject_move(fun, query_strings, query_group)
  return function()
    require("nvim-treesitter-textobjects.move")[fun](query_strings, query_group or "textobjects")
  end
end

local function goto_next_start(query_strings, query_group)
  return textobject_move("goto_next_start", query_strings, query_group)
end

local function goto_next_end(query_strings, query_group)
  return textobject_move("goto_next_end", query_strings, query_group)
end

local function goto_previous_start(query_strings, query_group)
  return textobject_move("goto_previous_start", query_strings, query_group)
end

local function goto_previous_end(query_strings, query_group)
  return textobject_move("goto_previous_end", query_strings, query_group)
end

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
      },
    },
    keys = {
      { "[m", goto_previous_start("@function.outer") },
      { "[M", goto_previous_end("@function.outer") },
      { "]m", goto_next_start("@function.outer") },
      { "]M", goto_next_end("@function.outer") },
      { "[c", goto_previous_start("@class.outer") },
      { "]c", goto_next_start("@class.outer") },
      { "]z", goto_next_start("@fold", "folds") },
    }
  },
  {
    'MeanderingProgrammer/treesitter-modules.nvim',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    opts = {
      ensure_installed = {
        "c", "lua", "vim", "vimdoc", "elixir", "javascript", "html", "zig", "go", "templ", "css", "nix",
        "xml", "bash", "diff", "http", "java", "make", "rust", "toml", "yaml", "gomod", "json5", "proto", "templ",
        "kotlin", "python", "svelte", "vimdoc", "comment", "clojure", "markdown", "starlark",
        "yaml", "helm"
      },
      fold = { enable = true },
      highlight = { enable = true },
      indent = { enable = true },
      incremental_selection = {
        enable = true,
        keymaps = {
          init_selection = "<cr>",
          node_incremental = "<cr>",
          scope_incremental = false,
          node_decremental = "<bs>",
        },
      },
    },
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
    end
  },
}
