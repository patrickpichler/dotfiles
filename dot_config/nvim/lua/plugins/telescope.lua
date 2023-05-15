return {
  {
    'nvim-telescope/telescope.nvim',

    event = { 'VeryLazy' },

    dependencies = {
      { 'nvim-lua/plenary.nvim' },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      { 'debugloop/telescope-undo.nvim' },
      { 'rcarriga/nvim-notify' },
    },

    config = function(_, opts)
      local telescope = require('telescope')

      telescope.setup(opts)
      telescope.load_extension('ui-select')
      telescope.load_extension('undo')
      telescope.load_extension('notify')
    end,

    keys = {
      {
        "<M-p>",
        function()
          require('telescope.builtin').find_files({
            find_command = { 'rg', '--files', '--hidden', '-g', '!.git' }
          })
        end
      },
      {
        '<space>g',
        function()
          require('telescope.builtin').live_grep()
        end
      }
    },

    cmd = {
      "Telescope",
    },

    opts = {
      defaults = {
        vimgrep_arguments = { 'rg', '--hidden', '--color=never',
          '--no-heading', '--with-filename', '--line-number',
          '--column', '--smart-case', '-g', '!.git' },
      },
    },
  },
}
