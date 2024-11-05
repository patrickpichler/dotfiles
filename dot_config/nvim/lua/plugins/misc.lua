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
