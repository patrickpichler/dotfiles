require('config.options')
require('config.keymaps')

vim.opt.background = 'light'

require("config.lazy") {
  dev = {
    path = '~/development/projects'
  }
}
