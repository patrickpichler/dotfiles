return {
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
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    opts = {
      library = {
        -- See the configuration section for more details
        -- Load luvit types when the `vim.uv` word is found
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      },
    },
  },

  {
    "obsidian-nvim/obsidian.nvim",
    version = "*", -- use latest release, remove to use latest commit
    ---@module 'obsidian'
    ---@type obsidian.config
    opts = {
      legacy_commands = false, -- this will be removed in 4.0.0
      workspaces = {
        {
          name = "personal",
          path = "~/vaults/personal",
        },
        {
          name = "work",
          path = "~/vaults/work",
        },
      },
    },
  }
}
