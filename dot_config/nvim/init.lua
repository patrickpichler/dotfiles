require("config.autocmds")
require("config.commands")
require("config.keymaps")
require("config.options")

vim.opt.background = "dark"

require("config.lazy") {
  dev = {
    path = "~/development/projects"
  }
}
