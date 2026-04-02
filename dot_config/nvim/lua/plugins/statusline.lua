return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = {
      { "nvim-tree/nvim-web-devicons" },
      { "stevearc/overseer.nvim" },
    },
    opts = function()
      local dmode_enabled = false
      vim.api.nvim_create_autocmd("User", {
        pattern = "DebugModeChanged",
        callback = function(args)
          dmode_enabled = args.data.enabled
        end
      })

      return {
        options = {
          globalstatus = true,
        },
        extensions = {
          "trouble",
          "lazy",
        },
        sections = {
          lualine_a = {
            {
              "mode",
              fmt = function(str) return dmode_enabled and "DEBUG" or str end,
            },
          },
          lualine_c = {
            { "filename", file_status = true, path = 1 },
            { "overseer" }
          }
        },
      }
    end,
  }
}
