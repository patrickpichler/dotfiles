return {
  {
    "nvim-telescope/telescope.nvim",

    event = { "VeryLazy" },

    dependencies = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope-ui-select.nvim" },
      { "debugloop/telescope-undo.nvim" },
      { "rcarriga/nvim-notify" },
      { "nvim-telescope/telescope-live-grep-args.nvim" },
    },

    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")
      local lga_actions = require("telescope-live-grep-args.actions")

      telescope.setup({
        defaults = {
          vimgrep_arguments = { "rg", "--hidden", "--color=never",
            "--no-heading", "--with-filename", "--line-number",
            "--column", "--smart-case", "-g", "!.git", },
          path_display = { "truncate" },
          layout_strategy = "vertical",
          layout_config = {
            preview_cutoff = 1,
            prompt_position = "top",
          },
          mappings = {
            i = {
              ["<C-y>"] = actions.select_default,
            },
            n = {
              ["<C-y>"] = actions.select_default,
            }
          },
        },
        extensions = {
          live_grep_args = {
            auto_quoting = true, -- enable/disable auto-quoting
            -- define mappings, e.g.
            mappings = {
              -- extend mappings
              i = {
                ["<C-k>"] = lga_actions.quote_prompt(),
                ["<C-g>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
                ["<C-f>"] = lga_actions.quote_prompt({ postfix = " --fixed-strings" }),
              },
            },
          }
        }
      })
      telescope.load_extension("ui-select")
      telescope.load_extension("undo")
      telescope.load_extension("notify")
      vim.cmd("autocmd User TelescopePreviewerLoaded setlocal number")
    end,

    keys = {
      {
        "<leader>/",
        function()
          -- You can pass additional configuration to telescope to change theme, layout, etc.
          require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
            winblend = 10,
            previewer = false,
          })
        end,
        desc = "[/] Fuzzily search in current buffer"
      },
      {
        "<leader><space>",
        function() require("telescope.builtin").buffers() end,
        desc = "[ ] Find existing buffers",
      },
      {
        "<leader>?",
        function() require("telescope.builtin").oldfiles() end,
        desc = "[?] Find recently opened files",
      },
      {
        "<leader>sf",
        function()
          require("telescope.builtin").find_files({
            find_command = { "rg", "--files", "--hidden", "-g", "!.git" }
          })
        end,
        desc = "[s]earch [f]iles"
      },
      {
        "<leader>sG",
        function() require("telescope").extensions.live_grep_args.live_grep_args() end,
        desc = "[s]earch [G]rep",
      },
      {
        "<leader>sw",
        function() require("telescope.builtin").grep_string() end,
        desc = "[s]earch [w]ord",
      },
      {
        "<leader>sh",
        function() require("telescope.builtin").help_tags() end,
        desc = "[s]earch [h]elp",
      },
      {
        "<leader>sc",
        function() require("telescope-live-grep-args.shortcuts").grep_word_under_cursor({ quote = false, postfix = "" }) end,
        desc = "Grep current words"
      },
      {
        "<leader>sb",
        function() require("telescope.builtin").buffers() end,
        desc = "[s]earch [b]uffers"
      },
      {
        "<leader>sr",
        function() require("telescope.builtin").resume() end,
        desc = "[s]earch [r]esume"
      },

      {
        "<leader>sgb",
        function() require("telescope.builtin").git_branch() end,
        desc = "[s]earch [g]it [b]ranch",
      },
      {
        "<leader>sgc",
        function() require("telescope.builtin").git_commit() end,
        desc = "[s]earch [g]it [c]ommit",
      },
      {
        "<leader>sgs",
        function() require("telescope.builtin").git_status() end,
        desc = "[s]earch [g]it [s]tatus",
      },
      {
        "<leader>sgS",
        function() require("telescope.builtin").git_stash() end,
        desc = "[s]earch [g]it [S]tash",
      },
      {
        "<leader>sgf",
        function() require("telescope.builtin").git_files() end,
        desc = "[s]earch [g]it [f]iles",
      },
    },

    cmd = {
      "Telescope",
    },
  },
}
