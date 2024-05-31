local M = {}
local group

M._debounce = function(ms, fn)
  local running = false
  return function()
    if running then
      return
    end
    vim.defer_fn(function()
      running = false
    end, ms)
    running = true
    vim.schedule(fn)
  end
end

if vim.loop.os_uname().sysname == "Darwin" then
  --- @return string background either "dark" or "light"
  function M.get_background()
    local result = vim.system({ "defaults", "read", "-g", "AppleInterfaceStyle" }, { text = true }):wait()
    local theme = vim.trim(result.stdout)

    return theme == "Dark" and "dark" or "light"
  end
else
  --- @return string background either "dark" or "light"
  function M.get_background()
    return "dark"
  end
end

function M.set_bg()
  vim.o.background = M.get_background()
end

function M._is_darwin()
  return vim.loop.os_uname().sysname == "Darwin"
end

function M.is_supported()
  return M._is_darwin()
end

function M.arm()
  if not M.is_supported() then
    return
  end
  group = vim.api.nvim_create_augroup("BackgroundWatch", { clear = true })
  -- TODO(patrick.pichler): figure out a better way of doing this, as it is not working with wezterm
  vim.api.nvim_create_autocmd("Signal", {
    pattern = "SIGWINCH",
    callback = M._debounce(500, M.set_bg),
    group = group,
  })
end

function M.disarm()
  if not group then
    return
  end

  vim.api.nvim_del_augroup_by_id(group)
  group = nil
end

return M
