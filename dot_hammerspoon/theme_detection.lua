---@class M
---@field _watcher? hs.distributednotifications
---@field _handlers fun(boolean)[]
local M = {
  _watcher = nil,
  _handlers = {},
}

local function getDarkModeFromSystem()
  local _, darkmode = hs.osascript.javascript("Application('System Events').appearancePreferences.darkMode.get()")
  return darkmode
end

function M:_initialize()
  -- exit if already watching
  if self._watcher ~= nil then return end

  self._watcher = hs.distributednotifications.new(function(_, _, _)
    local darkMode = getDarkModeFromSystem()

    for _, handler in ipairs(self._handlers) do
      handler(darkMode)
    end
  end, 'AppleInterfaceThemeChangedNotification')

  self._watcher:start()
end

M:_initialize()

---@param handler fun(boolean)
function M:addHandler(handler)
  table.insert(M._handlers, handler)
end

return M
