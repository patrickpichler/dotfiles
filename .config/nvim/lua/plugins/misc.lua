return {
  {
    'rcarriga/nvim-notify',

    config = function()
      local notify = require('notify')
      vim.notify = notify

      vim.keymap.set('n', '<leader>nd', function()
        notify.dismiss()
      end)
      vim.keymap.set('n', '<leader>no', function()
        vim.cmd(":Telescope notify")
      end)
    end,
  },

  { 'AndrewRadev/linediff.vim', },
  { 'junegunn/vim-peekaboo', },
  { 'machakann/vim-highlightedyank', },

  {
    'nvim-tree/nvim-web-devicons',
    opts = true,
  },
}
