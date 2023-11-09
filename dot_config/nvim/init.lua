require('config.options')
require('config.keymaps')

vim.opt.background = 'dark'

require("config.lazy") {
  dev = {
    path = '~/development/projects'
  }
}
