-- Moved here from options to ensure keymaps using it are working correct.
vim.g.mapleader = ","

-- easier transition from terminal mode to normal mode
vim.keymap.set('t', '<ESC>', '<C-\\><C-n>', { remap = false })

--" send escape even the escape key is remapped
vim.keymap.set('t', '<C-v><Esc>', '<Esc>', { remap = false })
vim.keymap.set({ 'n', 'v', 'x' }, '<C-c>', '<Esc>', { remap = true })

-- Make C-P C-N behave the same as Up Down in command mode
vim.keymap.set('c', '<C-p>', '<Up>', { remap = false })
vim.keymap.set('c', '<C-n>', '<Down>', { remap = false })

vim.keymap.set('n', '<M-h>', [[<cmd>vertical resize +5<cr>]], { remap = false })
vim.keymap.set('n', '<M-l>', [[<cmd>vertical resize -5<cr>]], { remap = false })
vim.keymap.set('n', '<M-k>', [[<cmd>horizontal resize +5<cr>]], { remap = false })
vim.keymap.set('n', '<M-j>', [[<cmd>horizontal resize -5<cr>]], { remap = false })

vim.keymap.set('n', '<leader>cp', function()
  local path = vim.fn.expand("%:.")
  vim.fn.setreg("+", path)
  vim.notify('Copied "' .. path .. '" to the clipboard!')
end, { remap = false, desc = 'Copy buffer path' })
