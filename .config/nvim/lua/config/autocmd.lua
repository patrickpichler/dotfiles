local function augroup(name)
  return vim.api.nvim_create_augroup("config_" .. name, { clear = true })
end

local additional_ft = augroup("additional_ft")

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "Jenkinsfile" },
  group = additional_ft,
  callback = function()
    vim.opt.ft = "groovy"
  end
})

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "*.terraformrc", "terraform.rc" },
  group = additional_ft,
  callback = function()
    vim.opt.ft = "hcl"
  end
})

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "*.tfstate" },
  group = additional_ft,
  callback = function()
    vim.opt.ft = "json"
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "terraform-vars" },
  group = additional_ft,
  callback = function()
    vim.opt.ft = "terraform"
  end
})
