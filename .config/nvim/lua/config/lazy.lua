local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

return function(opts)
  opts = vim.tbl_deep_extend("force", {
    spec = {
      { import = "plugins" },
    },
    performance = {
      cache = {
        enabled = true,
      },
      rtp = {
        disabled_plugins = {
          -- "matchit",
          -- "matchparen",
          "netrwPlugin",
          "rplugin",
          "tohtml",
          "tutor",
        },
      },
    },
  }, opts or {})

  require("lazy").setup(opts)
end
