vim.g.mapleader = ","

vim.opt.hidden = true
vim.opt.backspace = '2'
vim.opt.compatible = false
vim.opt.number = true

vim.opt.hlsearch = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2

vim.opt.smarttab = true
vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.termencoding = "utf-8"
vim.opt.fileencoding = "utf-8"

vim.opt.foldmethod = 'syntax'
vim.opt.foldlevelstart = 99
vim.opt.foldnestmax = 10
vim.opt.foldenable = false
vim.opt.foldlevel = 1

vim.opt.splitbelow = true
vim.opt.splitright = true

if vim.fn.has('termguicolors') then
  vim.opt.termguicolors = true
end

vim.opt.mouse = ''

vim.opt.undofile = true

if vim.fn.executable('par') then
  vim.opt.formatprg = 'par -re'
end
