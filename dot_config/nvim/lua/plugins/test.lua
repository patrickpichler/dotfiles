return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-go"
    },

    opts = {},

    config = function()
      -- get neotest namespace (api call creates or returns namespace)
      local neotest_ns = vim.api.nvim_create_namespace("neotest")

      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message =
                diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)

      local neotest = require("neotest")

      neotest.setup({
        adapters = {
          require("neotest-go")({
            experimental = {
              test_table = true,
            },
          }),
        },
      })

      function opts(desc)
        return {
          silent = true,
          desc = desc,
        }
      end

      vim.keymap.set('n', '<leader>tt', ':Neotest summary toggle<CR>', opts('Neo[t]est summary [t]oggle'))
      vim.keymap.set('n', '<leader>to', ':Neotest output-panel toggle<CR>', opts('Neo[t]est [t]oggle output-panel'))
      vim.keymap.set('n', '<leader>trr', ':Neotest run<CR>', opts('neo[t]est [r]un nea[r]est'))
      vim.keymap.set('n', '<leader>trf', ':Neotest run file<CR>', opts('Neo[t]est [r]un [f]ile'))
      vim.keymap.set('n', '<leader>trl', ':Neotest run last<CR>', opts('Neo[t]est [r]un [l]ast'))
    end,
  },
}
