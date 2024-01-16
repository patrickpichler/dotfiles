-- easier transition from terminal mode to normal mode
vim.keymap.set('t', '<ESC>', '<C-\\><C-n>', { remap = false })

--" send escape even the escape key is remapped
vim.keymap.set('t', '<C-v><Esc>', '<Esc>', { remap = false })

vim.keymap.set({'n', 'v', 'x'}, '<C-c>', '<Esc>', { remap = true })

-- Make C-P C-N behave the same as Up Down in command mode
vim.keymap.set('c', '<C-p>', '<Up>', { remap = false })
vim.keymap.set('c', '<C-n>', '<Down>', { remap = false })
