vim.filetype.add({
  extension = {
    terraformrc = 'hcl',
    tfstate = 'json',
    tfvars = 'hcl',
  },
  filename = {
    ['terraform.rc'] = 'hcl',
  },
})
