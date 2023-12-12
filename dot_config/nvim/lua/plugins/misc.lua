return {
  {
    'rcarriga/nvim-notify',

    config = function()
      local notify = require('notify')
      vim.notify = notify

      vim.keymap.set('n', '<leader>nd', function()
        notify.dismiss({ pending = true, silent = true })
      end, { desc = "Clear notifications" })
      vim.keymap.set('n', '<leader>no', function()
        vim.cmd(":Telescope notify")
      end, { desc = "Open notifications" })
    end,
  },

  { 'AndrewRadev/linediff.vim', },
  { 'junegunn/vim-peekaboo', },

  {
    'nvim-tree/nvim-web-devicons',
    opts = true,
  },

}
