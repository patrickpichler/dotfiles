require("config.autocmds")
require("config.commands")
require("config.keymaps")
require("config.options")
require("config.autocolor")
require("config.diagnostics")

require("config.lazy") {
  dev = {
    path = "~/development/projects"
  }
}
