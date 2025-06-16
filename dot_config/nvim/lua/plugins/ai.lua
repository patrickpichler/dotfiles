return {
  {
    "olimorris/codecompanion.nvim",

    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },

    opts = {
      strategies = {
        chat = {
          adapter = "anthropic",
          keymaps = {
            close = {
              modes = {
                n = "<C-q>",
                i = "<C-q>",
              },
              index = 4,
              callback = "keymaps.close",
              description = "Close Chat",
            },
          },
        },
        inline = {
          adapter = "anthropic",
        },
        cmd = {
          adapter = "anthropic",
        },
      },
      adapters = {
        gemini = function()
          return require("codecompanion.adapters").extend("gemini", {
            env = {
              api_key = "cmd:gopass show -o google/aistudio",
            },
          })
        end,
        anthropic = function()
          return require("codecompanion.adapters").extend("anthropic", {
            env = {
              api_key = "cmd:gopass show -o anthropic/local-dev",
            },
            schema = {
              model = {
                default = "claude-3-5-haiku-20241022",
              },
            },
          })
        end,
      },
    },

    keys = {
      { "<leader>at", ":CodeCompanionChat toggle<CR>", desc = "Toggle CodeCompanion Chat" },
    },
  },
}
