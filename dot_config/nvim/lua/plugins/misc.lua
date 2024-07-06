return {
  {
    "rcarriga/nvim-notify",

    config = function()
      local notify = require("notify")
      vim.notify = notify

      vim.keymap.set("n", "<leader>nd", function()
        notify.dismiss({ pending = true, silent = true })
      end, { desc = "Clear notifications" })
      vim.keymap.set("n", "<leader>no", function()
        vim.cmd(":Telescope notify")
      end, { desc = "Open notifications" })
    end,
  },

  { "AndrewRadev/linediff.vim", },

  {
    "nvim-tree/nvim-web-devicons",
    opts = true,
  },

  {
    "Olical/conjure",

    init = function()
      vim.g["conjure#mapping#doc_word"] = false
      vim.g["conjure#client_on_load"] = false
    end,
  },

  {
    "mrjones2014/smart-splits.nvim",
    lazy = false,
    init = function()
      -- recommended mappings
      -- resizing splits
      -- these keymaps will also accept a range,
      -- for example `10<A-h>` will `resize_left` by `(10 * config.default_amount)`
      vim.keymap.set("n", "<A-h>", require("smart-splits").resize_left)
      vim.keymap.set("n", "<A-j>", require("smart-splits").resize_down)
      vim.keymap.set("n", "<A-k>", require("smart-splits").resize_up)
      vim.keymap.set("n", "<A-l>", require("smart-splits").resize_right)
      -- moving between splits
      vim.keymap.set("n", "<C-h>", require("smart-splits").move_cursor_left)
      vim.keymap.set("n", "<C-j>", require("smart-splits").move_cursor_down)
      vim.keymap.set("n", "<C-k>", require("smart-splits").move_cursor_up)
      vim.keymap.set("n", "<C-l>", require("smart-splits").move_cursor_right)
      -- swapping buffers between windows
      vim.keymap.set("n", "<leader><leader>h", require("smart-splits").swap_buf_left)
      vim.keymap.set("n", "<leader><leader>j", require("smart-splits").swap_buf_down)
      vim.keymap.set("n", "<leader><leader>k", require("smart-splits").swap_buf_up)
      vim.keymap.set("n", "<leader><leader>l", require("smart-splits").swap_buf_right)
    end,
  },

  {
    "stevearc/overseer.nvim",
    opts = {},

    cmd = {
      "OverseerOpen",
      "OverseerClose",
      "OverseerToggle",
      "OverseerSaveBundle",
      "OverseerLoadBundle",
      "OverseerDeleteBundle",
      "OverseerRunCmd",
      "OverseerRun",
      "OverseerInfo",
      "OverseerBuild",
      "OverseerQuickAction",
      "OverseerTaskAction",
      "OverseerClearCache",
    },

    init = function()
      vim.keymap.set("n", "<leader>ot", ":OverseerToggle! right<cr>", { silent = true })
      vim.keymap.set("n", "<leader>or", ":OverseerRun<cr>", { silent = true })
      vim.keymap.set("n", "<leader>oqa", ":OverseerQuickAction<cr>", { silent = true })
      vim.keymap.set("n", "<leader>oa", ":OverseerTaskAction<cr>", { silent = true })
    end,
  },

  {
    "folke/neodev.nvim",

    opts = {
      library = {
        plugins = {
          "neotest"
        },

        types = true
      },
    },
  },
}
