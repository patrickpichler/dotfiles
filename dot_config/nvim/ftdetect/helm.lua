local function is_helm_file(path)
  local check = vim.fs.find("Chart.yaml", { path = vim.fs.dirname(path), upward = true })
  return not vim.tbl_isempty(check)
end

--@private
--@return string
local function yaml_filetype(path, bufname)
  return is_helm_file(path) and "helm.yaml" or "yaml"
end

--@private
--@return string
local function tmpl_filetype(path, bufname)
  return is_helm_file(path) and "helm.tmpl" or "template"
end

--@private
--@return string
local function tpl_filetype(path, bufname)
  return is_helm_file(path) and "helm.tmpl" or "smarty"
end

vim.filetype.add({
  extension = {
    yaml = yaml_filetype,
    yml = yaml_filetype,
    tmpl = tmpl_filetype,
    tpl = tpl_filetype
  },
  filename = {
    ["Chart.yaml"] = "yaml",
    ["Chart.lock"] = "yaml",
  }
})
