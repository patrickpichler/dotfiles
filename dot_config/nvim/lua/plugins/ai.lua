return {
  {
    enabled = false,
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
  {
    "NickvanDyke/opencode.nvim",
    dependencies = {
      -- Recommended for `ask()` and `select()`.
      -- Required for `snacks` provider.
      ---@module 'snacks' <- Loads `snacks.nvim` types for configuration intellisense.
      { "folke/snacks.nvim" },
    },
    config = function()
      ---@type opencode.Opts
      vim.g.opencode_opts = {
        -- Your configuration, if any — see `lua/opencode/config.lua`, or "goto definition".
      }

      -- Required for `opts.events.reload`.
      vim.o.autoread = true
    end,
    keys = {
      { "<leader>a",  nil,                                                                  desc = "AI/OpenCode" },
      { "<leader>ac", function() require("opencode").ask("@this: ", { submit = true }) end, desc = "Ask" },
      { "<leader>ax", function() require("opencode").select() end,                          desc = "Execute opencode action..." },
      { "<leader>af", function() require("opencode").toggle() end,                          desc = "Toggle" },
      { "<leader>ao", function() require("opencode").operator("@this ") end,                desc = "Add range" },
      { "<leader>ao", function() return require("opencode").operator("@this ") .. "_" end,  desc = "Add line" },
    },
  },
}
