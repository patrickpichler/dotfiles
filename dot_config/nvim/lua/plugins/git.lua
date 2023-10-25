local function toggleGitBlame()
  local lastWinNr = vim.fn.winnr("$")
  local found = false
  print(lastWinNr)

  for winnr = 0, lastWinNr do
    if vim.fn.getbufvar(vim.fn.winbufnr(winnr), '&filetype') == 'fugitiveblame' then
      vim.fn.execute(winnr .. 'close')
      found = true
    end
  end

  if not found then
    vim.cmd.Git('blame')
  end
end

return {
  { 'rhysd/committia.vim' },

  {
    'ruifm/gitlinker.nvim',

    dependencies = {
      'nvim-lua/plenary.nvim',
    },

    opts = true,
  },

  {
    'lewis6991/gitsigns.nvim',
    opts = {
      update_debounce = 500,
      on_attach = function(bufnr)
        local gs = require('gitsigns')

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map('n', ']c', function()
          if vim.wo.diff then return ']c' end
          vim.schedule(function() gs.next_hunk() end)
          return '<Ignore>'
        end, { expr = true, desc = "Next hunk" })

        map('n', '[c', function()
          if vim.wo.diff then return '[c' end
          vim.schedule(function() gs.prev_hunk() end)
          return '<Ignore>'
        end, { expr = true, desc = "Previous hunk" })

        -- Actions
        map('n', '<leader>hs', gs.stage_hunk, { desc = "Stage hunk" })
        map('n', '<leader>hr', gs.reset_hunk, { desc = "Reset hunk" })
        map('v', '<leader>hs', function() gs.stage_hunk { vim.fn.line("."), vim.fn.line("v") } end,
          { desc = "Stage hunk" })
        map('v', '<leader>hr', function() gs.reset_hunk { vim.fn.line("."), vim.fn.line("v") } end,
          { desc = "Reset hunk" })
        map('n', '<leader>hS', gs.stage_buffer, { desc = "Stage buffer" })
        map('n', '<leader>hu', gs.undo_stage_hunk, { desc = "Undo stage hunk" })
        map('n', '<leader>hR', gs.reset_buffer, { desc = "Reset buffer" })
        map('n', '<leader>hp', gs.preview_hunk, { desc = "Preview hunk" })
        map('n', '<leader>hb', function() gs.blame_line { full = true } end, { desc = "Toggle blame line" })
        map('n', '<leader>tb', gs.toggle_current_line_blame, { desc = "Toggle blame current line" })
        map('n', '<leader>hd', gs.diffthis, { desc = "Diff this" })
        map('n', '<leader>hD', function() gs.diffthis('~') end, { desc = "Diff this head" })
        map('n', '<leader>td', gs.toggle_deleted, { desc = "Toggle deleted" })

        -- Text object
        map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
      end
    },
  },

  {
    'tpope/vim-fugitive',
    keys = {
      { '<leader>gb', toggleGitBlame, desc = 'Toggle Git blame' },
    },
    cmd = 'Git',
  },

}
