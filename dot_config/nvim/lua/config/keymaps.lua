-- easier transition from terminal mode to normal mode
vim.keymap.set('t', '<ESC>', '<C-\\><C-n>', { remap = false })

--" send escape even the escape key is remapped
vim.keymap.set('t', '<C-v><Esc>', '<Esc>', { remap = false })

vim.keymap.set('', '<C-c>', '<Esc>', { remap = false })

vim.keymap.set('n', '<C-h>', '<C-w>h', { remap = false })
vim.keymap.set('n', '<C-j>', '<C-w>j', { remap = false })
vim.keymap.set('n', '<C-k>', '<C-w>k', { remap = false })
vim.keymap.set('n', '<C-l>', '<C-w>l', { remap = false })

-- Make C-P C-N behave the same as Up Down in command mode
vim.keymap.set('c', '<C-p>', '<Up>', { remap = false })
vim.keymap.set('c', '<C-n>', '<Down>', { remap = false })

vim.keymap.set('n', '[g', ':diffget //2<cr>', { remap = false, silent = true })
vim.keymap.set('n', ']g', ':diffget //3<cr>', { remap = false, silent = true })

vim.keymap.set('n', '<M-Up>', ':res -2<cr>', { remap = false, silent = true })
vim.keymap.set('n', '<M-Down>', ':res +2<cr>', { remap = false, silent = true })
vim.keymap.set('n', '<M-Left>', ':vertical res -2<cr>', { remap = false, silent = true })
vim.keymap.set('n', '<M-Right>', ':vertical res +2<cr>', { remap = false, silent = true })
