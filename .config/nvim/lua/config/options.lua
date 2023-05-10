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

vim.o.foldcolumn = '1' -- '0' is not bad
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true

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
