return {
  {
    'nvim-telescope/telescope.nvim',

    event = { 'VeryLazy' },

    dependencies = {
      { 'nvim-lua/plenary.nvim' },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      { 'debugloop/telescope-undo.nvim' },
      { 'rcarriga/nvim-notify' },
      { 'nvim-telescope/telescope-live-grep-args.nvim' },
    },

    config = function(_, opts)
      local telescope = require('telescope')
      local lga_actions = require("telescope-live-grep-args.actions")

      telescope.setup({
        defaults = {
          vimgrep_arguments = { 'rg', '--hidden', '--color=never',
            '--no-heading', '--with-filename', '--line-number',
            '--column', '--smart-case', '-g', '!.git', },
          path_display = {'truncate'},
        },
        extensions = {
          live_grep_args = {
            auto_quoting = true, -- enable/disable auto-quoting
            -- define mappings, e.g.
            mappings = {
              -- extend mappings
              i = {
                ["<C-k>"] = lga_actions.quote_prompt(),
                ["<C-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
                ["<C-f>"] = lga_actions.quote_prompt({ postfix = " --fixed-strings" }),
              },
            },
          }
        }
      })
      telescope.load_extension('ui-select')
      telescope.load_extension('undo')
      telescope.load_extension('notify')
    end,

    keys = {
      {
        "<M-p>",
        function()
          require('telescope.builtin').find_files({
            find_command = { 'rg', '--files', '--hidden', '-g', '!.git' }
          })
        end
      },
      {
        '<space>g',
        function()
          require('telescope').extensions.live_grep_args.live_grep_args()
        end
      }
    },

    cmd = {
      "Telescope",
    },
  },
}
