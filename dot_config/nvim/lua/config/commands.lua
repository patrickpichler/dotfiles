-- big kudos to MariaSolOs for the awesome idea
vim.api.nvim_create_user_command('Scratch', function(opts)
  local filetype = opts.fargs[1] or 'scratch'

  vim.cmd 'bel 10new'

  local buf = vim.api.nvim_get_current_buf()

  for name, value in pairs {
    filetype = filetype,
    buftype = 'nowrite',
    bufhidden = 'hide',
    swapfile = false,
    modifiable = true,
  } do
    vim.api.nvim_set_option_value(name, value, { buf = buf })
  end

  vim.keymap.set('n', 'q', '<cmd>quit<cr>', { buffer = buf })
end, { desc = 'Open a scratch buffer', nargs = '?' })
