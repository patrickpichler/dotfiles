return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = {
      { "nvim-tree/nvim-web-devicons" },
      { "stevearc/overseer.nvim" },
    },
    opts = function()
      return {
        extensions = {
          "neo-tree",
          "fugitive",
          "trouble",
          "lazy",
        },
        sections = {
          lualine_c = {
            { "filename", file_status = true, path = 1 },
            { "overseer" }
          }
        },
      }
    end,
  }
}
